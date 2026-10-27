use anyhow::Result;
use git2::{DiffDelta, DiffHunk, Repository};
use ruff_source_file::{LineIndex, OneIndexed, SourceFile};
use ruff_text_size::{Ranged, TextRange, TextSize};
use serde::Deserialize;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    str::FromStr,
};

use crate::diagnostics::{Diagnostic, Fix, SourceMap};
use crate::fs::{self};

/// Type exposed to user for input
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Deserialize)]
pub struct Filter {
    inner: Vec<LineFilter>,
}

impl FromStr for Filter {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let inner: Vec<LineFilter> = serde_json::from_str(s)?;
        Ok(Self { inner })
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Deserialize)]
pub struct LineFilter {
    name: PathBuf,
    lines: Option<Vec<LineRange>>,
}

impl FromStr for LineFilter {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Deserialize)]
pub struct LineRange {
    start: OneIndexed,
    end: OneIndexed,
}

impl LineRange {
    pub fn new(start: OneIndexed, end: OneIndexed) -> Self {
        Self { start, end }
    }
}

/// Parse a line range from `START:END` (1-based, inclusive), or a single line `LINE`
impl FromStr for LineRange {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parse = |n: &str| -> anyhow::Result<OneIndexed> {
            let n: usize = n.trim().parse()?;
            OneIndexed::new(n).ok_or_else(|| anyhow::anyhow!("line numbers start at 1"))
        };
        let (start, end) = match s.split_once(':') {
            Some((start, end)) => (parse(start)?, parse(end)?),
            None => {
                let line = parse(s)?;
                (line, line)
            }
        };
        if end < start {
            anyhow::bail!("end of line range ({end}) is before its start ({start})");
        }
        Ok(Self { start, end })
    }
}

/// Type used internally for filtering
///
/// Maps filenames onto sets of offset ranges
#[derive(Debug, Clone)]
pub struct FilterMap {
    inner: HashMap<PathBuf, Vec<LineRange>>,
}

impl FilterMap {
    /// Construct a `FilterMap`, converting from line ranges to offset ranges
    pub fn new(value: Filter) -> Self {
        let inner = value
            .inner
            .into_iter()
            .map(|line_filter| {
                (
                    fs::normalize_path(line_filter.name),
                    line_filter.lines.unwrap_or_default(),
                )
            })
            .collect();
        Self { inner }
    }

    pub fn files(&self) -> Vec<&PathBuf> {
        self.inner.keys().collect()
    }

    /// Construct a `FilterMap` for a single file
    pub fn from_lines<P: Into<PathBuf>>(file: P, lines: Vec<LineRange>) -> Self {
        let inner = HashMap::from([(file.into(), lines)]);
        Self { inner }
    }

    pub fn contains<P: Into<PathBuf>>(&self, file: P) -> bool {
        self.inner.contains_key(&file.into())
    }

    /// Return offset ranges for filename, if it exists
    ///
    /// Requires a `SourceFile` to turn line numbers into offsets
    pub fn get<P: Into<PathBuf>>(&self, file: P, source: &SourceFile) -> Option<FilterSet> {
        self.inner
            .get(&file.into())
            .map(|range| FilterSet::from_source_file(range, source))
    }
}

/// Set of offset ranges into a file
#[derive(Debug, Clone)]
pub struct FilterSet {
    inner: Vec<TextRange>,
}

impl FilterSet {
    /// A `FilterSet` that contains nothing
    pub fn empty() -> Self {
        Self { inner: vec![] }
    }

    pub fn new(value: Vec<LineRange>, line_index: &LineIndex, contents: &str) -> Self {
        let inner: Vec<_> = value
            .into_iter()
            .map(|line_range| {
                let start = line_index.line_start(line_range.start, contents);
                let end = line_index.line_end(line_range.end, contents);

                TextRange::new(start, end)
            })
            .collect();
        Self { inner }
    }

    pub fn from_source_file(value: &[LineRange], source: &SourceFile) -> Self {
        let code = source.to_source_code();

        if value.is_empty() {
            return Self {
                inner: vec![TextRange::new(
                    TextSize::new(0),
                    TextSize::try_from(code.text().len()).unwrap(),
                )],
            };
        }

        let inner: Vec<_> = value
            .iter()
            .map(|line_range| {
                let start = code.line_start(line_range.start);
                let end = code.line_end(line_range.end);
                TextRange::new(start, end)
            })
            .collect();
        Self { inner }
    }

    /// Check if this `FilterSet` contains an offset.
    ///
    /// Ends of line ranges are considered included
    pub fn contains(&self, offset: TextSize) -> bool {
        self.inner
            .iter()
            .any(|range| range.contains_inclusive(offset))
    }

    pub fn contains_range(&self, other: TextRange) -> bool {
        self.inner.iter().any(|range| range.contains_range(other))
    }

    /// Check if every edit of a fix lies within this `FilterSet`
    pub fn contains_fix(&self, fix: &Fix) -> bool {
        fix.edits()
            .iter()
            .all(|edit| self.contains_range(edit.range()))
    }

    /// Only keep diagnostics that start within this `FilterSet`
    pub fn retain_diagnostics(&self, diagnostics: &mut Vec<Diagnostic>) {
        diagnostics.retain(|diagnostic| {
            self.contains(
                diagnostic
                    .primary_span()
                    .and_then(|span| span.range())
                    .map(|range| range.start())
                    .unwrap_or_default(),
            )
        });
    }

    /// Move the offset ranges to account for the edits recorded in
    /// `source_map`, so that they cover the same code after fixes have been
    /// applied. Text inserted at the boundary of a range is considered part of
    /// that range.
    pub fn remap(&mut self, source_map: &SourceMap) {
        // Markers come in (start, end) pairs, one per applied edit, in order
        let edits: Vec<_> = source_map
            .markers()
            .as_chunks::<2>()
            .0
            .iter()
            .map(|[start, end]| (start, end))
            .collect();

        let map_offset = |offset: TextSize, is_end: bool| -> TextSize {
            let mut new_offset = offset;
            for (start, end) in &edits {
                let is_insertion = start.source() == end.source();
                if offset < start.source()
                    || (offset == start.source() && !(is_end && is_insertion))
                {
                    // Edit is after this offset, and so are all remaining edits
                    break;
                }
                if offset < end.source() {
                    // Offset is strictly inside the edit, so snap to its edge
                    return if is_end { end.dest() } else { start.dest() };
                }
                // Edit is entirely before this offset
                new_offset = offset - end.source() + end.dest();
            }
            new_offset
        };

        for range in &mut self.inner {
            *range = TextRange::new(
                map_offset(range.start(), false),
                map_offset(range.end(), true),
            );
        }
    }
}

/// Create a line filter for just the files currently staged in the index
pub fn git_staged_files<P: AsRef<Path>>(project_root: P) -> Result<FilterMap> {
    let repo = Repository::open_from_env()?;

    let head_tree = repo.head()?.peel_to_tree()?;
    let diff = repo.diff_tree_to_index(Some(&head_tree), None, None)?;

    // Bit annoying, but the `FnMut` trait is still nightly, so we can't
    // implement it directly on `HunkCb`, so we need a closure that captures our
    // state and then calls it
    let mut hunk_state = HunkCb::new(project_root.as_ref());
    let mut hunk_cb =
        |diff_delta: DiffDelta, hunk: DiffHunk| -> bool { hunk_state.call(diff_delta, hunk) };

    diff.foreach(&mut file_cb, None, Some(&mut hunk_cb), None)?;

    Ok(hunk_state.filter)
}

/// Create a filter for files that have changed since ``treeish``
pub fn git_since<P: AsRef<Path>>(treeish: &str, project_root: P) -> Result<FilterMap> {
    let repo = Repository::open_from_env()?;

    // TODO(peter): nicer error message if treeish isn't correct
    let object = repo.revparse_single(treeish)?;
    let tree = object.peel_to_tree()?;
    let diff = repo.diff_tree_to_workdir(Some(&tree), None)?;

    let mut hunk_state = HunkCb::new(project_root.as_ref());
    let mut hunk_cb =
        |diff_delta: DiffDelta, hunk: DiffHunk| -> bool { hunk_state.call(diff_delta, hunk) };

    diff.foreach(&mut file_cb, None, Some(&mut hunk_cb), None)?;

    Ok(hunk_state.filter)
}

/// Callback for hunks with `Diff::foreach`
///
/// Let's us reuse the same closure between the `git_staged_files` and
/// `git_since`
struct HunkCb<'a> {
    filter: FilterMap,
    project_root: &'a Path,
}

impl<'a> HunkCb<'a> {
    fn new(project_root: &'a Path) -> Self {
        let inner = HashMap::new();
        let filter = FilterMap { inner };
        Self {
            filter,
            project_root,
        }
    }

    fn call(&mut self, diff_delta: DiffDelta, hunk: DiffHunk) -> bool {
        if let Some(file) = diff_delta.new_file().path() {
            let start = OneIndexed::from_zero_indexed(hunk.new_start() as usize);
            let end = OneIndexed::from_zero_indexed((hunk.new_start() + hunk.new_lines()) as usize);
            let range = LineRange { start, end };
            let file_abs = fs::fully_normalize_path_to(file, self.project_root);
            self.filter
                .inner
                .entry(file_abs)
                .and_modify(|f| f.push(range.clone()))
                .or_insert(vec![range]);
        }

        true
    }
}

fn file_cb(_diff_delta: DiffDelta, _progress: f32) -> bool {
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    use anyhow::{Context, Result};
    use lazy_static::lazy_static;
    use ruff_source_file::SourceFileBuilder;
    use ruff_text_size::TextSize;

    const TEST_FILE: &str = r#"line 1
line 2
line 3
line 4
line 5
line 6
line 7
line 8
line 9
line 10
"#;

    lazy_static! {
        static ref SOURCE_FILE: SourceFile =
            SourceFileBuilder::new("file1.f90", TEST_FILE).finish();
    }

    fn make_filter() -> Filter {
        Filter {
            inner: vec![
                LineFilter {
                    name: "file1.f90".into(),
                    lines: Some(vec![
                        LineRange {
                            start: OneIndexed::new(1).unwrap(),
                            end: OneIndexed::new(3).unwrap(),
                        },
                        LineRange {
                            start: OneIndexed::new(8).unwrap(),
                            end: OneIndexed::new(10).unwrap(),
                        },
                    ]),
                },
                LineFilter {
                    name: "file2.f90".into(),
                    lines: None,
                },
            ],
        }
    }

    #[test]
    fn from_string() -> Result<()> {
        let example = r#"[
  {"name": "file1.f90", "lines": [[1, 3], [8, 10]]},
  {"name": "file2.f90"}
]"#;

        let filter = Filter::from_str(example)?;

        assert_eq!(filter, make_filter());

        Ok(())
    }

    #[test]
    fn filter_map() -> Result<()> {
        let filter = FilterMap::new(make_filter());

        assert!(
            filter
                .get(fs::normalize_path("file1.f90"), &SOURCE_FILE)
                .is_some()
        );
        assert!(filter.get("nothing", &SOURCE_FILE).is_none());
        Ok(())
    }

    #[test]
    fn filter_set() -> Result<()> {
        let filter = FilterMap::new(make_filter());
        let filter = filter
            .get(fs::normalize_path("file1.f90"), &SOURCE_FILE)
            .context("Expected file1.f90")?;

        assert!(filter.contains(TextSize::new(18)));
        assert!(filter.contains(TextSize::new(52)));
        assert!(!filter.contains(TextSize::new(25)));
        assert!(!filter.contains(TextSize::new(129)));

        assert!(filter.contains_range(TextRange::new(TextSize::new(6), TextSize::new(17))));
        assert!(!filter.contains_range(TextRange::new(TextSize::new(32), TextSize::new(38))));

        Ok(())
    }

    #[test]
    fn line_range_from_str() -> Result<()> {
        let range: LineRange = "3:7".parse()?;
        assert_eq!(
            range,
            LineRange::new(OneIndexed::new(3).unwrap(), OneIndexed::new(7).unwrap())
        );
        let range: LineRange = "4".parse()?;
        assert_eq!(
            range,
            LineRange::new(OneIndexed::new(4).unwrap(), OneIndexed::new(4).unwrap())
        );
        assert!("0:3".parse::<LineRange>().is_err());
        assert!("5:3".parse::<LineRange>().is_err());
        assert!("a:b".parse::<LineRange>().is_err());
        Ok(())
    }

    #[test]
    fn filter_set_remap() {
        use crate::diagnostics::Edit;

        // Ranges [10, 20] and [30, 40]
        let mut filter = FilterSet {
            inner: vec![
                TextRange::new(TextSize::new(10), TextSize::new(20)),
                TextRange::new(TextSize::new(30), TextSize::new(40)),
            ],
        };

        // Apply three edits, mimicking `apply_fixes`:
        // - insert 2 characters at 10 (start of first range)
        // - delete [15, 18) inside first range
        // - insert 5 characters at 40 (end of second range)
        let edits = [
            Edit::insertion("ab".to_string(), TextSize::new(10)),
            Edit::deletion(TextSize::new(15), TextSize::new(18)),
            Edit::insertion("cdefg".to_string(), TextSize::new(40)),
        ];
        let mut source_map = SourceMap::default();
        let mut output_len = TextSize::new(0);
        let mut last_pos = TextSize::new(0);
        for edit in &edits {
            output_len += edit.start() - last_pos;
            source_map.push_start_marker(edit, output_len);
            output_len += TextSize::try_from(edit.content().unwrap_or_default().len()).unwrap();
            source_map.push_end_marker(edit, output_len);
            last_pos = edit.end();
        }

        filter.remap(&source_map);

        // Insertions at range boundaries are kept inside the range
        assert_eq!(
            filter.inner,
            vec![
                TextRange::new(TextSize::new(10), TextSize::new(19)),
                TextRange::new(TextSize::new(29), TextSize::new(44)),
            ]
        );
    }
}
