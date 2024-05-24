mod logging;
mod util;
mod follow_algorithm;
mod temporal_graph;

use chrono::{DateTime, Utc}; // 0.4.15
use clap::{Arg, Parser};
use csv;
use derive_more::Add;
use indicatif::{ParallelProgressIterator, ProgressBar};
use rand::rngs::StdRng;
use rand::{Rng, RngCore, SeedableRng};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::Serialize;
use std::fs::File;
use std::ops::AddAssign;
use std::time::SystemTime;
use std::{error::Error, fs, io};

use rand::prelude::SliceRandom;
use std::io::BufRead;
use log::{error, info};

use temporal_graph::common::*;
use follow_algorithm::*;
use logging::*;

#[derive(Debug, Serialize)]
struct ExperimentResult {
    probability: f64,
    node_count: usize,
    edge_count: usize,
    restarts: usize,
    restarts_for_component_discovery: usize,
    tmax: usize,
    skipped_redundant_infections: bool,
}

fn write_results(results: &Vec<ExperimentResult>) -> Result<(), Box<dyn Error>> {
    fs::create_dir_all("./data")?;
    let now = SystemTime::now();
    let now_utc: DateTime<Utc> = now.into();
    let filename = format!("./data/experiment-results-{}.csv", now_utc.to_rfc3339());
    println!("Writing results to {}", &filename);
    let file = File::create(&filename)?;
    let mut wtr = csv::Writer::from_writer(file);

    for result in results {
        wtr.serialize(result)?;
    }

    wtr.flush()?;

    Ok(())
}

const MAX_NODES: usize = 500;
const REPEATS: usize = 5;
const PROBABILITIES: [f64; 7] = [0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9];

#[derive(Debug, Clone)]
struct ExperimentTask {
    graph: TemporalGraph,
    probability: f64,
}

fn generate_erdos_renyi_tasks(args: &Args) -> Vec<ExperimentTask> {
    let iterations = MAX_NODES * REPEATS * PROBABILITIES.len();
    let mut tasks = Vec::with_capacity(iterations);

    // For deterministic graph generation
    let mut shared_rng: Box<dyn RngCore> = Box::new(StdRng::seed_from_u64(args.seed));

    for probability in PROBABILITIES {
        for nodes in 1..=MAX_NODES {
            for _repeat in 1..=REPEATS {
                tasks.push(ExperimentTask {
                    // While deterministically generating graph is significantly slower, it makes
                    // comparing results between different configurations of the follow algorithm
                    // much more meaningful
                    graph: TemporalGraph::gen_erdos_renyi_with_rng(
                        nodes,
                        Time(nodes),
                        probability,
                        &mut shared_rng,
                    ),
                    probability: probability,
                });
            }
        }
    }

    tasks.shuffle(&mut shared_rng);

    tasks
}

fn generate_snap_resistance_csv_tasks(args: &Args) -> io::Result<Vec<ExperimentTask>> {
    let mut tasks = Vec::with_capacity(args.from_snap_resistance_files.len());
    for path in &args.from_snap_resistance_files {
        tasks.push(ExperimentTask {
            graph: TemporalGraph::from_snap_resistance_csv(path)?,
            probability: f64::NAN,
        })
    }
    Ok(tasks)
}

/// Simulate Infections on Temporal Graphs
#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
struct Args {
    /// If a node is marked for a start infection, but all adjacent edges are discovered, don't skip
    #[arg(short, long)]
    dont_skip_redundant_start_infections: bool,
    #[arg(short, long, default_value_t = 42)]
    seed: u64,
    #[arg(long, num_args = 1..10000)]
    from_snap_resistance_files: Vec<String>,
}

impl Args {
    fn default_with_seed(seed: u64) -> Args {
        Args {
            dont_skip_redundant_start_infections: false,
            from_snap_resistance_files: Vec::new(),
            seed,
        }
    }

    fn erdos_renyi(seed: u64, dont_skip_redundant_start_infections: bool) -> Args {
        Args {
            dont_skip_redundant_start_infections,
            seed,
            from_snap_resistance_files: Vec::new(),
        }
    }
}

impl Default for Args {
    fn default() -> Self {
        Args {
            dont_skip_redundant_start_infections: false,
            seed: 42,
            from_snap_resistance_files: Vec::new(),
        }
    }
}

fn main() -> io::Result<()> {
    init_logging();

    let args = Args::parse();
    info!("Arguments: {:?}", args);

    let tasks = if args.from_snap_resistance_files.is_empty() {
        generate_erdos_renyi_tasks(&args)
    } else {
        generate_snap_resistance_csv_tasks(&args)?
    };

    let bar = ProgressBar::new(tasks.len() as u64);

    let results: Vec<ExperimentResult> = tasks
        .par_iter()
        .progress_with(bar)
        .map(move |task: &ExperimentTask| {
            let mut execution = FollowAlgorithmExecution::new(task.graph.clone(), args.clone());
            execution.execute();
            ExperimentResult {
                probability: task.probability,
                node_count: execution.graph.node_count(),
                edge_count: execution.graph.edge_count(),
                restarts: execution.number_of_restarts(),
                restarts_for_component_discovery: execution.restarts_for_component_discovery,
                tmax: task.graph.tmax.0,
                skipped_redundant_infections: !args.dont_skip_redundant_start_infections,
            }
        })
        .collect();

    write_results(&results).unwrap();

    Ok(())
}

// General TODOS
// - [ ] Write tests
// - [ ] Better Experimental Setup
// - [ ] Write logging with sensible levels
// - [ ] Add CLI arguments
// - [ ] Write export to file
// - [ ] Include experimental metadata in files
// - [ ] Index Vecs by newytpes
// - [x] rewrite past_start_infections to use HashMap
// - Generalize to directed graphs (and parse SNAP dataset accordingly)
