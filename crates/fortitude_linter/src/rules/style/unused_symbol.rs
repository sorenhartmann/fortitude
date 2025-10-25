use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::sync::OnceLock;

use crate::ast::FortitudeNode;
use crate::settings::Settings;
use crate::{AstRule, FromAstNode};
use chrono::offset;
use itertools::Itertools;
use log::warn;
use rayon::iter::plumbing::UnindexedProducer;
use rayon::{option, vec};
use ruff_diagnostics::{Diagnostic, Edit, Fix, Violation};
use ruff_macros::{ViolationMetadata, derive_message_formats};
use ruff_source_file::{LineRanges, SourceFile};
use ruff_text_size::{TextRange, TextSize, TextSlice};
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
// TODO: assignment-only for non-out variables??
    // some way of annotating declarations as dummy-out?
// TODO: private/public?
// TODO: Imported types
// TODO: type fields
// TODO: Parameters
// TODO: Aliased imports
// TODO: dummy args as unsafe fix? - e.g. interfaces no longer applies
// TODO: Descriptions?
// TODO: Message?

static QUERY: OnceLock<Query> = OnceLock::new();

fn get_query() -> &'static Query {
    QUERY.get_or_init(|| {
        Query::new(
            &tree_sitter_fortran::LANGUAGE.into(),
            "
            [(subroutine) (function) (module)] @scope

            ((comment) @omp_directive
                (#match? @omp_directive \"\\![$][oO][mM][pP]\"))

            (variable_declaration
                declarator: (identifier) @decl)

            (parameters
                (identifier) @decl)

            (use_statement
                (included_items
                    (identifier) @decl))

            (identifier) @use
            ",
        )
        .unwrap()
    })
}

struct Symbol {
    name: String,
    range: TextRange,
}

impl Violation for UnusedSymbol {
    #[derive_message_formats]
    fn message(&self) -> String {
        format!("{}", self.msg)
    }
}
struct ParseOutcome<T> {
    result: T,
    offset: usize, // Number of characters read
}

fn parse_omp_args(comment_node: &Node, src: &str) -> (Vec<Symbol>, Vec<Symbol>) {

    let mut used_symbols: Vec<String> = vec![];
    let mut declared_symbols: Vec<String> = vec![];

    let range = comment_node.byte_range();

    let line = &src[range.clone()];

    // Skip initial [!$omp[ ]]
    dbg!(&line.chars().enumerate().skip_while(|(i, c)| !c.is_whitespace()).collect_vec());
    todo!()
    // line.find()
    // todo!();

    // let Some((_, mut line)) = line.split_once(char::is_whitespace) else {
    //     return (vec![], vec![]);
    // };

    // // I think i just have to do the parsing myself
    // // - a bunch more work is required for fixes
    // // - any more advanced parsing should probably be done via treesitter
    // while let Some(ParseOutcome{result: (clause, args), offset: new_offset}) = parse_clause(line, offset) {
    //     if matches!(clause, "private" | "shared" | "firstprivate") {
    //         declared_symbols.extend(args);
    //     } else {
    //         used_symbols.extend(args);
    //     }
    //     offset = new_offset;
    // }
    // dbg!(declared_symbols);
    // dbg!(used_symbols);

    // todo!();
}

fn parse_clause(line: &str, offset: usize) -> Option<ParseOutcome<(&str, Vec<String>)>> {

   let claused_line = &line[offset..];

    if line.is_empty() || line.starts_with('&') {
        return None
    }

    let clause_end = line
        .find(|c: char| c.is_whitespace() || c == '(' || c == '&')
        .map(|i| i - 1)
        .unwrap_or(line.len());

    let (clause, _) = line.split_at(clause_end);

    let name = clause.to_lowercase();
    todo!();

    // match parse_clause_args(rest) {
    //     None => Some(ParseOutcome {
    //         result: (name, vec![]),
    //         rest,
    //     }),
    //     Some(outcome) => Some(ParseOutcome {
    //         result: (name, outcome.result),
    //         offset: o
    //     }),
    // }
}

fn parse_clause_args(line: &'_ str) -> Option<ParseOutcome<Vec<String>>> {
    let line = line.trim_start();
    if !line.starts_with('(') {
        return None;
    }
    todo!();
    let mut level = 0;
    let mut seps = vec![];
    let mut args_end = None;
    for (i, c) in line.chars().enumerate() {
        match c {
            '(' => {
                level += 1;
            }
            ')' => {
                level -= 1;
            }
            ',' => {
                seps.push(i);
            }
            _ => {}
        }
        if level == 0 {
            args_end.replace(i);
            break;
        }
    }

    let args_end = args_end?;

    // let args = seps
    //     .iter()
    //     .chain(once(&args_end))
    //     .scan(1, |i: &mut usize, &j| {
    //         let arg = line[*i..j].trim_start().to_lowercase();
    //         *i = j + 1;
    //         Some(arg)
    //     })
    //     .collect();

    // Some(ParseOutcome {
    //     result: args,
    //     rest: line[args_end + 1..].trim_start(),
    // })
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


        // dbg!(&src_.lines());
        // parse_omp_args(src);

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
                        "use" => {
                            identifiers
                                .entry(n.to_text(src_).unwrap().to_lowercase())
                                .or_default()
                                .push(n);
                        }
                        "omp_directive" => {
                            parse_omp_args(&n, src_);
                            dbg!(&n.to_text(src_));
                        }
                        _ => {
                            unreachable!("unexpected capture name")
                        }
                    }
                })
            });

        todo!();

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
