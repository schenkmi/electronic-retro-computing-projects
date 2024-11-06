use anyhow::{anyhow, Result};
use clap::builder::PossibleValue;
use clap::{Parser, Subcommand, ValueEnum};
use indicatif;
use indicatif::ProgressBar;
use indicatif::ProgressStyle;
use std::fs;
use std::iter;
use std::path::{Path, PathBuf};
use std::time::Duration;

use picolink::*;

mod rom_size;
use crate::rom_size::*;

fn read_file(name: &Path, rom_size: RomSize) -> Result<Vec<u8>> {
    let mut data = fs::read(name)?;
    if data.len() > rom_size.bytes() {
        return Err(anyhow!(
            "{:?} larger ({}) than rom size ({})",
            name,
            data.len(),
            rom_size.bytes()
        ));
    }

    let diff = rom_size.bytes() - data.len();
    data.extend(iter::repeat(0u8).take(diff));

    Ok(data.repeat(RomSize::MBit(2).bytes() / rom_size.bytes()))
}

#[derive(Clone, Debug, Copy)]
pub enum ResetKind {
    Low,
    High,
    Z,
}

impl ValueEnum for ResetKind {
    fn value_variants<'a>() -> &'a [Self] {
        &[
            ResetKind::Low,
            ResetKind::High,
            ResetKind::Z
        ]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        match self {
            ResetKind::High => Some(PossibleValue::new("high")),
            ResetKind::Low => Some(PossibleValue::new("low")),
            ResetKind::Z => Some(PossibleValue::new("z")),
        }
    }
}

#[derive(Debug, Parser)] // requires `derive` feature
#[command(name = "picorom")]
#[command(about = "PicoROM controller", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Return a list of currently connected PicoROM devices.
    List,

    /// Flash the activity LED on a specific PicoRom
    Identify {
        /// PicoROM device name.
        name: String,
    },

    /// Commit the current ROM image to flash memory
    Commit {
        /// PicoROM device name.
        name: String,
    },

    /// Change the name of a PicoROM device.
    Rename {
        /// Current name.
        current: String,
        /// New name to rename it to.
        new: String,
    },

    /// Upload a ROM image to a PicoROM
    Upload {
        /// PicoROM device name.
        name: String,
        /// Path of file to upload.
        source: PathBuf,
        /// Emulate a specific ROM size.
        #[arg(value_enum, ignore_case=true, default_value_t=RomSize::MBit(2))]
        size: RomSize,
        /// Store the uploaded image in flash memory also.
        #[arg(short, long, default_value_t = false)]
        store: bool,
    },

    Reset {
        /// PicoROM device name.
        name: String,

        /// Reset Output
        kind: ResetKind,
    }
}

fn main() -> Result<()> {
    let args = Cli::parse();

    match args.command {
        Commands::List => {
            let found = enumerate_picos()?;
            if found.len() > 0 {
                println!("Available PicoROMs:");
                for k in found.keys() {
                    println!("  {}", k);
                }
            } else {
                println!("No PicoROMs found.");
            }
        }
        Commands::Identify { name } => {
            let mut pico = find_pico(&name)?;
            pico.identify()?;
            println!("Requested identification from '{}'", name);
        }
        Commands::Commit { name } => {
            let mut pico = find_pico(&name)?;
            let spinner = ProgressBar::new_spinner()
                .with_prefix("Storing to Flash")
                .with_style(
                    ProgressStyle::with_template("{prefix:.bold} {spinner} {msg}")
                        .unwrap()
                        .tick_chars(r"\|/--"),
                );
            spinner.enable_steady_tick(Duration::from_millis(250));
            pico.commit_rom()?;
            spinner.finish_with_message("Done.");
        }
        Commands::Rename { current, new } => {
            let mut pico = find_pico(&current)?;
            pico.set_ident(&new)?;
            println!("Renamed '{}' to '{}'", current, new);
        }
        Commands::Upload {
            name,
            source,
            size,
            store,
        } => {
            let mut pico = find_pico(&name)?;
            let data = read_file(source.as_path(), size)?;
            let progress = ProgressBar::new(data.len() as u64)
                .with_prefix("Uploading ROM")
                .with_style(
                    ProgressStyle::with_template("{prefix:.bold} [{wide_bar:.cyan/blue}] {msg:10}")
                        .unwrap()
                        .progress_chars("#>-"),
                );
            pico.upload(&data, size.mask(), |x| progress.inc(x as u64))?;
            progress.finish_with_message("Done.");
            if store {
                let spinner = ProgressBar::new_spinner()
                    .with_prefix("Storing to Flash")
                    .with_style(
                        ProgressStyle::with_template("{prefix:.bold} {spinner} {msg}")
                            .unwrap()
                            .tick_chars(r"\|/--"),
                    );
                spinner.enable_steady_tick(Duration::from_millis(250));
                pico.commit_rom()?;
                spinner.finish_with_message("Done.");
            }
        }
        Commands::Reset { name, kind } => {
            let mut pico = find_pico(&name)?;
            let lib_kind = match kind {
                ResetKind::High => picolink::ResetKind::High,
                ResetKind::Low => picolink::ResetKind::Low,
                ResetKind::Z => picolink::ResetKind::Z,
            };

            pico.reset(lib_kind)?;
            println!("Setting '{}' reset pin to: {:?}", name, kind);
        }
    }

    Ok(())
}
