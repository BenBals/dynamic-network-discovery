mod follow_algorithm;
mod logging;
mod temporal_graph;
mod util;

use chrono::{DateTime, Utc}; // 0.4.15
use clap::Parser;
use csv;
use indicatif::{ParallelProgressIterator, ProgressBar};
use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::Serialize;
use std::fs::File;
use std::time::SystemTime;
use std::{error::Error, fs, io};

use log::info;
use more_asserts::assert_ge;
use rand::prelude::SliceRandom;

use follow_algorithm::*;
use logging::*;
use temporal_graph::common::*;

#[derive(Debug, Serialize)]
struct ExperimentResult {
    probability: f64,
    node_count: usize,
    edge_count: usize,
    restarts: usize,
    restarts_for_component_discovery: usize,
    tmax: usize,
    delta: usize,
    skipped_redundant_infections: bool,
    component_count: usize,
    component_max_size: usize,
    component_mean_size: f64,
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

const MAX_NODES: usize = 300;
const NODES_STEP_SIZE: usize = 10;
const REPEATS: usize = 100;
const PROBABILITIES: [f64; 12] = [
    0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.5, 0.7, 0.9,
];
const TMAX_FACTORS: [f64; 26] = [
    0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.5, 2.0, 2.5,
    3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0,
];

const DELTA_FACTORS: [f64; 8] = [0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5];

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
        for nodes in (1..=MAX_NODES).step_by(NODES_STEP_SIZE) {
            for tmax_factor in TMAX_FACTORS {
                for delta_factor in DELTA_FACTORS {
                    for _repeat in 1..=REPEATS {
                        let tmax = (nodes as f64 * tmax_factor).floor().max(2.0) as usize;
                        let delta= (tmax as f64 * delta_factor).floor().max(1.0) as usize;
                        tasks.push(ExperimentTask {
                            // While deterministically generating graphs is significantly slower, it makes
                            // comparing results between different configurations of the follow algorithm
                            // much more meaningful
                            graph: TemporalGraph::gen_erdos_renyi_with_rng(
                                nodes,
                                Time(tmax),
                                Time(delta),
                                probability,
                                &mut shared_rng,
                            ),
                            probability: probability,
                        });
                    }
                }
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
            let components = task.graph.delta_egde_connected_components();
            let largest_component = if task.graph.edge_count() > 0 {
                components.iter().map(Vec::len).max().unwrap()
            } else {
                0
            };
            ExperimentResult {
                probability: task.probability,
                node_count: execution.graph.node_count(),
                edge_count: execution.graph.edge_count(),
                restarts: execution.number_of_restarts(),
                restarts_for_component_discovery: execution.restarts_for_component_discovery,
                tmax: task.graph.tmax.0,
                delta: task.graph.delta.0,
                skipped_redundant_infections: !args.dont_skip_redundant_start_infections,
                component_count: components.len(),
                component_max_size: largest_component,
                component_mean_size: (components.iter().map(Vec::len).sum::<usize>() as f64)
                    / components.len() as f64,
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
// - [ ] Track negative information (especially with multiedges)
