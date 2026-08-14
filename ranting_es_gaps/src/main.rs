//! `ranting-es-gaps` — check `ranting_es`'s closed Spanish lexicon against real Spanish text.
//!
//! ```text
//! ranting-es-gaps <path>... [--out DIR] [--limit N]
//! ```
//!
//! Paths may be files or directories (recursed). Output is a `failures/` tree; see
//! [`crate::report`] for the layout and [`crate::probes`] for what is checked and why.
//!
//! **This crate depends on both `ranting` and `ranting_es`.** That does not compromise
//! `ranting_es`'s own public-API-only falsifier contract — that contract is a property of
//! `ranting_es/Cargo.toml` alone (it depends on `ranting` and nothing else) and is unaffected by
//! who else depends on `ranting_es`. This crate's job is the same shape as `ranting_gaps`'s
//! relative to `ranting`: it inspects a crate from outside as a development tool, not as a
//! falsifier. See README.md's "Not a falsifier" section.
//!
//! Unlike `ranting_gaps`, this crate never nominates a case from the corpus — `ranting_es`'s
//! lexicon is closed, so every comparison is enumerated directly from it, and the corpus only
//! grades confidence. There is accordingly no `--min-occurrences` (nothing to filter as noise)
//! and no `--unattested` (nothing is ever hidden — the `Confidence` column in each case already
//! says whether the corpus backs it up). See `crate::corpus`'s module doc for why.

mod corpus;
mod finding;
mod probes;
mod report;
mod spanish;

use std::path::PathBuf;
use std::process::ExitCode;

const USAGE: &str = "\
ranting-es-gaps — check ranting_es's closed Spanish lexicon against real Spanish text

USAGE:
    ranting-es-gaps <path>... [OPTIONS]

ARGS:
    <path>...    Spanish text files or directories to read (directories are recursed)

OPTIONS:
    --out DIR     Where to write the report      [default: failures]
    --limit N     Max cases listed per cause      [default: 40]
    -h, --help    Print this message

No corpus ships with this crate — point it at any Spanish text you have (a Tatoeba `spa.txt`
dump, a Spanish Wikipedia extract, or your own prose).
";

struct Args {
    paths: Vec<PathBuf>,
    out: PathBuf,
    limit: usize,
}

fn parse_args() -> Result<Option<Args>, String> {
    let mut paths = Vec::new();
    let mut out = PathBuf::from("failures");
    let mut limit = 40usize;

    let mut it = std::env::args().skip(1);
    while let Some(arg) = it.next() {
        match arg.as_str() {
            "-h" | "--help" => return Ok(None),
            "--out" => out = it.next().ok_or("--out needs a value")?.into(),
            "--limit" => {
                limit = it
                    .next()
                    .ok_or_else(|| "--limit needs a value".to_string())
                    .and_then(|v| v.parse::<usize>().map_err(|e| format!("--limit: {e}")))?
            }
            other if other.starts_with('-') => return Err(format!("unknown option {other}")),
            other => paths.push(PathBuf::from(other)),
        }
    }
    if paths.is_empty() {
        return Err("no input paths given".to_string());
    }
    Ok(Some(Args { paths, out, limit }))
}

fn main() -> ExitCode {
    let args = match parse_args() {
        Ok(Some(args)) => args,
        Ok(None) => {
            print!("{USAGE}");
            return ExitCode::SUCCESS;
        }
        Err(e) => {
            eprintln!("ranting-es-gaps: {e}\n\n{USAGE}");
            return ExitCode::FAILURE;
        }
    };

    let corpus = match corpus::read(&args.paths) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("ranting-es-gaps: reading input: {e}");
            return ExitCode::FAILURE;
        }
    };
    if corpus.total_words == 0 {
        eprintln!("ranting-es-gaps: no readable text found in the given paths");
        return ExitCode::FAILURE;
    }

    let findings = probes::run_all(&corpus, args.limit);
    let run = report::RunInfo {
        sources: &args.paths,
        files: corpus.files,
        total_words: corpus.total_words,
    };
    if let Err(e) = report::write(&args.out, &findings, &run) {
        eprintln!("ranting-es-gaps: writing {}: {e}", args.out.display());
        return ExitCode::FAILURE;
    }

    println!(
        "read {} words from {} file(s); wrote {} cause(s) to {}/",
        corpus.total_words,
        corpus.files,
        findings.len(),
        args.out.display()
    );
    for f in &findings {
        println!(
            "  {:<30} {:>6} attested occurrences  {:>4} case(s)",
            f.id,
            f.occurrences(),
            f.cases.len()
        );
    }
    ExitCode::SUCCESS
}
