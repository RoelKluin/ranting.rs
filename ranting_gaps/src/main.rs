//! `ranting-gaps` — read arbitrary English text, report what `ranting` cannot inflect.
//!
//! ```text
//! ranting-gaps <path>... [--out DIR] [--min-occurrences N] [--limit N]
//! ```
//!
//! Paths may be files or directories (recursed). Output is a `failures/` tree; see
//! [`crate::report`] for the layout and [`crate::probes`] for what is checked and what was
//! deliberately left out.
//!
//! **This crate is a development tool, not a falsifier.** `ranting_i18n` and `ranting_es` depend
//! on `ranting` alone by contract, because their whole purpose is to prove the *public* API is
//! sufficient for a non-English fork. This crate has the opposite job — it inspects `ranting`'s
//! behavior from outside and needs `ranting_core::ph_ext` as an oracle for the closed pre-noun
//! vocabulary — so its `ranting_core` dependency is deliberate and is not a precedent for the
//! falsifiers.

mod corpus;
mod english;
mod finding;
mod probes;
mod report;

use std::path::PathBuf;
use std::process::ExitCode;

const USAGE: &str = "\
ranting-gaps — report what ranting cannot inflect, with corpus frequency evidence

USAGE:
    ranting-gaps <path>... [OPTIONS]

ARGS:
    <path>...    Text files or directories to read (directories are recursed)

OPTIONS:
    --out DIR              Where to write the report      [default: failures]
    --min-occurrences N    Ignore words seen in noun position fewer than N times
                           (corpus noise filter)          [default: 2]
    --limit N              Max cases listed per cause     [default: 40]
    --unattested           Also report corrections the corpus never writes. Off by
                           default: attestation is what keeps non-nouns out of the
                           plural findings without needing a POS tagger.
    -h, --help             Print this message
";

struct Args {
    paths: Vec<PathBuf>,
    out: PathBuf,
    min_occurrences: usize,
    limit: usize,
    unattested: bool,
}

fn parse_args() -> Result<Option<Args>, String> {
    let mut paths = Vec::new();
    let mut out = PathBuf::from("failures");
    let mut min_occurrences = 2usize;
    let mut limit = 40usize;
    let mut unattested = false;

    let mut it = std::env::args().skip(1);
    while let Some(arg) = it.next() {
        let mut value = |name: &str| {
            it.next()
                .ok_or_else(|| format!("{name} needs a value"))
                .and_then(|v| v.parse::<usize>().map_err(|e| format!("{name}: {e}")))
        };
        match arg.as_str() {
            "-h" | "--help" => return Ok(None),
            "--out" => out = it.next().ok_or("--out needs a value")?.into(),
            "--min-occurrences" => min_occurrences = value("--min-occurrences")?,
            "--limit" => limit = value("--limit")?,
            "--unattested" => unattested = true,
            other if other.starts_with('-') => return Err(format!("unknown option {other}")),
            other => paths.push(PathBuf::from(other)),
        }
    }
    if paths.is_empty() {
        return Err("no input paths given".to_string());
    }
    Ok(Some(Args {
        paths,
        out,
        min_occurrences,
        limit,
        unattested,
    }))
}

fn main() -> ExitCode {
    let args = match parse_args() {
        Ok(Some(args)) => args,
        Ok(None) => {
            print!("{USAGE}");
            return ExitCode::SUCCESS;
        }
        Err(e) => {
            eprintln!("ranting-gaps: {e}\n\n{USAGE}");
            return ExitCode::FAILURE;
        }
    };

    let corpus = match corpus::read(&args.paths) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("ranting-gaps: reading input: {e}");
            return ExitCode::FAILURE;
        }
    };
    if corpus.total_words == 0 {
        eprintln!("ranting-gaps: no readable text found in the given paths");
        return ExitCode::FAILURE;
    }

    let findings = probes::run_all(&corpus, args.min_occurrences, args.limit, args.unattested);
    let run = report::RunInfo {
        sources: &args.paths,
        files: corpus.files,
        total_words: corpus.total_words,
        min_occurrences: args.min_occurrences,
    };
    if let Err(e) = report::write(&args.out, &findings, &run) {
        eprintln!("ranting-gaps: writing {}: {e}", args.out.display());
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
            "  {:<40} {:>6} occurrences  {:>4} word(s)",
            f.id,
            f.occurrences(),
            f.cases.len()
        );
    }
    ExitCode::SUCCESS
}
