use std::collections::{HashMap, HashSet};
use std::sync::OnceLock;

use crate::ast::FortitudeNode;
use crate::settings::Settings;
use crate::{AstRule, FromAstNode};
use log::warn;
use ruff_diagnostics::{Diagnostic, Edit, Fix, Violation};
use ruff_macros::{ViolationMetadata, derive_message_formats};
use ruff_source_file::SourceFile;
use ruff_text_size::TextSize;
use tree_sitter::{Node, Query, QueryCursor, StreamingIterator};

/// ## What it does
///
/// ## Why is this bad?
///
/// ## Example
/// ```f90
/// ```
///
/// Use instead:
/// ```f90
/// ```
#[derive(ViolationMetadata)]
pub(crate) struct UnusedSymbol {
    msg: String,
}

// TODO: In OMP statements?
// TODO: Imported types
// TODO: type fieldsA
// TODO: Parameters
// TODO: Aliased imports
// TODO: Descriptions?
// TODO: Message?

static QUERY: OnceLock<Query> = OnceLock::new();

fn get_query() -> &'static Query {
    QUERY.get_or_init(|| {
        Query::new(
            &tree_sitter_fortran::LANGUAGE.into(),
            "
            [(subroutine) (function) (module)] @scope

            (variable_declaration
                declarator: (identifier) @decl)



            (parameters
                (identifier) @decl)

            (use_statement
                (included_items
                    (identifier) @decl))

            (identifier) @ident",
        )
        .unwrap()
    })
}

impl Violation for UnusedSymbol {
    #[derive_message_formats]
    fn message(&self) -> String {
        format!("{}", self.msg)
    }
}

impl AstRule for UnusedSymbol {
    fn check<'a>(
        _settings: &Settings,
        node: &'a Node,
        src: &'a SourceFile,
    ) -> Option<Vec<Diagnostic>> {
        if node.child_count() == 0 {
            return None;
        }

        let src_ = src.source_text();

        let mut declarations: HashMap<String, Vec<_>> = HashMap::new();
        let mut identifiers: HashMap<String, Vec<_>> = HashMap::new();
        let mut scopes = HashSet::new();

        let query = get_query();
        let capture_names = query.capture_names();

        QueryCursor::new()
            .captures(query, *node, src_.as_bytes())
            .for_each(|(m, _)| {
                m.captures.iter().for_each(|x| {
                    let n = x.node;
                    match *capture_names.get(x.index as usize).unwrap_or(&"unknown") {
                        "scope" => {
                            scopes.insert(n);
                        }
                        "decl" => {
                            declarations
                                .entry(n.to_text(src_).unwrap().to_lowercase())
                                .or_default()
                                .push(n);
                        }
                        "ident" => {
                            identifiers
                                .entry(n.to_text(src_).unwrap().to_lowercase())
                                .or_default()
                                .push(n);
                        }
                        _ => {
                            warn!("unexpected capture name")
                        }
                    }
                })
            });

        // Identify identifiers as nodes that are not declarations
        identifiers.iter_mut().for_each(|(k, v)| {
            if let Some(d) = declarations.get(k) {
                v.retain(|n| !d.contains(n));
            };
        });
        identifiers.retain(|_, v| !v.is_empty());

        // Group by parent statement. If all unused, emit diagnostic/fix for parent
        let mut by_parent: HashMap<Node<'_>, Vec<_>> = HashMap::new();

        declarations.iter().for_each(|(name, decls)| {
            decls.iter().for_each(|decl| {
                let Some(scope) = decl.ancestors().find(|s| scopes.contains(s)) else {
                    // Abort if no scope is found
                    return;
                };

                // Abort if a corresponding identitifer is found in scope
                if identifiers.get(name).is_some_and(|idents| {
                    idents.iter().any(|ident| {
                        scope.start_byte() <= ident.start_byte()
                            && scope.end_byte() >= ident.end_byte()
                    })
                }) {
                    return;
                };

                // Abort if in interface blok
                if decl.ancestors().any(|n| n.kind() == "interface") {
                    return;
                }

                if let Some(parent) = decl.parent() {
                    by_parent.entry(parent).or_default().push(decl);
                }
            })
        });

        let mut diags = vec![];
        by_parent.iter().for_each(|(parent, decls)| {
            let mut siblings: Vec<_> = parent
                .children(&mut parent.walk())
                .filter(|n| n.kind() == "identifier")
                .collect();

            if siblings.len() == decls.len() {
                diags.push(
                    Diagnostic::from_node(
                        Self {
                            msg: format!("Unused declaration statement"),
                        },
                        parent,
                    )
                    .with_fix(Fix::safe_edit(parent.edit_delete(src))),
                );
            } else {
                siblings.sort_by_key(|n| n.start_byte());
                let mut is_initial = true;

                siblings.iter().for_each(|node| {
                    // generate safe independent edits
                    // e.g. y are used nodes, x are unused
                    // :: y[, x][, x], y[, x][, x]
                    // :: [x, ][x, ][x, ]y[, x][, x]
                    // i.e. when part of inital unused variables, include suffix else
                    // include prefix

                    let is_unused = decls.contains(&node);
                    is_initial = is_initial && is_unused;
                    if !is_unused {
                        return;
                    }
                    let a;
                    let b;

                    if is_initial {
                        a = node.start_byte();
                        b = node.end_byte()
                            + src_[node.end_byte()..]
                                .find(|c: char| !(c.is_whitespace() || c == ',' || c == '&'))
                                .unwrap();
                    } else {
                        a = node.start_byte()
                            - src_[..node.start_byte()]
                                .bytes()
                                .rev()
                                .position(|c| {
                                    !((c as char).is_whitespace() || c == b',' || c == b'&')
                                })
                                .unwrap();
                        b = node.end_byte();
                    }

                    diags.push(
                        Diagnostic::from_node(
                            Self {
                                msg: format!(
                                    "Unused variable: '{}'",
                                    node.to_text(src_).unwrap_or("UNKOWN")
                                ),
                            },
                            node,
                        )
                        .with_fix(Fix::safe_edit(Edit::deletion(
                            TextSize::new(a.try_into().unwrap()),
                            TextSize::new(b.try_into().unwrap()),
                        ))),
                    );
                });
            }
        });

        diags.sort_by_key(|x| x.range.start());
        Some(diags)

    }

    fn entrypoints() -> Vec<&'static str> {
        vec!["module"]
    }
}
