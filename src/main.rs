use chrono::{DateTime, Utc}; // 0.4.15
use clap::Parser;
use csv;
use derive_more::Add;
use indicatif::{ParallelProgressIterator, ProgressBar};
use rand::{thread_rng, Rng};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::Serialize;
use std::cmp::max;
use std::fs::File;
use std::ops::AddAssign;
use std::time::SystemTime;
use std::{error::Error, fs};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct NodeIdx(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct EdgeIdx(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Add)]
struct Time(usize);

#[derive(Debug, Clone)]
struct TemporalGraph {
    adj_lists: Vec<Vec<(NodeIdx, EdgeIdx)>>,
    edges: Vec<(NodeIdx, NodeIdx, Time)>,
    tmax: Time,
}

impl AddAssign<usize> for Time {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs
    }
}

impl TemporalGraph {
    fn gen_erdos_renyi(size: usize, tmax: Time, probability: f64) -> TemporalGraph {
        let mut adj_lists = vec![vec![]; size];

        let mut edges =
            Vec::with_capacity((probability * size as f64 * size as f64).ceil() as usize);
        let mut rng = thread_rng();

        for i in 0..size {
            for j in 0..i {
                if rng.gen_bool(probability) {
                    edges.push((NodeIdx(j), NodeIdx(i), Time(rng.gen_range(1..tmax.0))));

                    adj_lists[i].push((NodeIdx(j), EdgeIdx(edges.len() - 1)));
                    adj_lists[j].push((NodeIdx(i), EdgeIdx(edges.len() - 1)));
                }
            }
        }

        TemporalGraph {
            tmax,
            edges,
            adj_lists,
        }
    }

    fn node_count(&self) -> usize {
        self.adj_lists.len()
    }

    fn edge_count(&self) -> usize {
        self.edges.len()
    }

    fn edge_label(&self, edge: EdgeIdx) -> Time {
        self.edges[edge.0].2
    }

    fn neighbours(&self, node: NodeIdx) -> &Vec<(NodeIdx, EdgeIdx)> {
        &self.adj_lists[node.0]
    }

    fn is_edge_adjacent_to_node(&self, edge: EdgeIdx, node: NodeIdx) -> bool {
        (self.edges[edge.0].0 == node) | (self.edges[edge.0].1 == node)
    }

    fn from_edge_list(edges: Vec<(NodeIdx, NodeIdx, Time)>, tmax: Time) -> TemporalGraph {
        let largest_node_idx = edges.iter().map(|edge| edge.1 .0).max().unwrap_or(0);

        let mut adj_lists = vec![vec![]; largest_node_idx + 1];

        for (edge_id, (left, right, _time)) in edges.iter().enumerate() {
            adj_lists[left.0].push((right.clone(), EdgeIdx(edge_id)));
            adj_lists[right.0].push((left.clone(), EdgeIdx(edge_id)));
        }

        TemporalGraph {
            edges,
            tmax,
            adj_lists,
        }
    }
}

struct FollowAlgorithmExecution {
    graph: TemporalGraph,
    past_start_infections: Vec<Vec<Time>>,
    todo_start_infections: Vec<(NodeIdx, Time)>,
    discovered_edges: Vec<bool>,
    infection_attempts: Vec<Vec<bool>>,
    discovered_edges_count: usize,
    restarts_for_component_discovery: usize,
    args: Args,
}

impl FollowAlgorithmExecution {
    fn new(graph: TemporalGraph, args: Args) -> FollowAlgorithmExecution {
        Self {
            past_start_infections: vec![vec![]; graph.node_count()],
            discovered_edges: vec![false; graph.edge_count()],
            discovered_edges_count: 0,
            restarts_for_component_discovery: 0,
            infection_attempts: vec![vec![false; graph.tmax.0]; graph.edge_count()],
            todo_start_infections: vec![],
            graph,
            args,
        }
    }

    fn is_immune(&self, node: NodeIdx, infection_log: &Vec<Option<Time>>) -> bool {
        infection_log[node.0].is_some()
    }

    fn make_infection_attempt(
        &mut self,
        edge: (NodeIdx, EdgeIdx),
        time: Time,
        infection_log: &mut Vec<Option<Time>>,
    ) -> bool {
        let is_immune: bool = self.is_immune(edge.0, infection_log);
        if !is_immune {
            self.infection_attempts[edge.1 .0][time.0] = true;
        }
        if (self.graph.edge_label(edge.1) == time) && !is_immune {
            if !self.discovered_edges[edge.1 .0] {
                self.discovered_edges_count += 1;
                self.discovered_edges[edge.1 .0] = true;
            }
            infection_log[edge.0 .0] = Some(time);
            return true;
        }
        false
    }

    fn simulate_infection(
        &mut self,
        root_node: NodeIdx,
        infection_time: Time,
    ) -> (Vec<Option<Time>>, Vec<EdgeIdx>) {
        let mut infected: Vec<NodeIdx> = vec![root_node];
        let mut time = infection_time + Time(1);
        let mut infection_log = vec![None; self.graph.node_count()];
        let mut infected_edges = vec![];

        while !infected.is_empty() & (time.0 < self.graph.tmax.0) {
            let mut new_infected = vec![];
            for node in &infected {
                // TODO: Optimize the clone away
                for edge in self.graph.adj_lists[node.0].clone() {
                    if self.make_infection_attempt(edge.clone(), time, &mut infection_log) {
                        new_infected.push(edge.0);
                        infected_edges.push(edge.1);

                        self.update_todo_start_infections(edge.1)
                    }
                }
            }

            infected = new_infected;

            time += 1;
        }

        // TODO: Mark as edge as discovered if all but one times have been tried (remember to update todo_start_infections)

        (infection_log, infected_edges)
    }

    // Call this when the edge label has been discovered
    fn update_todo_start_infections(&mut self, edge: EdgeIdx) {
        let left = self.graph.edges[edge.0].0;
        let right = self.graph.edges[edge.0].0;
        let label = self.graph.edge_label(edge);

        self.add_todo_start_infection(left, label);
        if label.0 >= 1 {
            self.add_todo_start_infection(left, Time(label.0 - 1));
        }
        if label.0 >= 2 {
            self.add_todo_start_infection(left, Time(label.0 - 1 - 1));
        }

        self.add_todo_start_infection(right, label);
        if label.0 >= 1 {
            self.add_todo_start_infection(right, Time(label.0 - 1));
        }
        if label.0 >= 2 {
            self.add_todo_start_infection(right, Time(label.0 - 1 - 1));
        }
    }

    fn add_todo_start_infection(&mut self, node: NodeIdx, time: Time) {
        if !self.past_start_infections[node.0].contains(&time) {
            self.todo_start_infections.push((node, time))
        }
    }

    fn execute(&mut self) {
        while self.discovered_edges_count < self.graph.edge_count() {
            self.find_start_edge();
            self.explore_todo_start_infections();
        }
    }

    fn find_start_edge(&mut self) -> EdgeIdx {
        for edge in 0..self.graph.edge_count() {
            if self.discovered_edges[edge] == false {
                // println!("Trying to find a start edge at {:?}", self.graph.edges[edge].0);
                return self
                    .find_new_edge_starting_at_node(self.graph.edges[edge].0)
                    .unwrap();
            }
        }

        panic!("This should never happen")
    }

    fn find_new_edge_starting_at_node(&mut self, start_node: NodeIdx) -> Option<EdgeIdx> {
        // TODO: Optimize the clone away
        for neighbour in self.graph.neighbours(start_node).clone() {
            // println!("\tAttempting to find edge between {:?} and {:?}", start_node, neighbour);
            // println!("\t\tPreviously discovered: {:?}", self.discovered_edges);
            if !self.discovered_edges[neighbour.1 .0] {
                // Edges at time 0 can never be discovered, because we cannot cause a matching start infection
                for time in 1..self.graph.tmax.0 {
                    // println!("\t\tTesting time {}", time);
                    if !self.infection_attempts[neighbour.1 .0][time] {
                        self.restarts_for_component_discovery += 1;
                        self.past_start_infections[neighbour.0 .0].push(Time(time - 1));
                        let (infection_log, infected_edges) =
                            self.simulate_infection(start_node, Time(time - 1));
                        // println!("\t\t\tInfected edges: {:?}", infected_edges);

                        // TODO: If still neccessary, update todo_start_infections here

                        // TODO: Optimize to not iterate as much
                        for edge in infected_edges {
                            if self.discovered_edges[edge.0] {
                                return Some(edge);
                            }
                        }
                    }
                }
            }
        }

        None
    }

    fn count_missing_edges_at_node(&self, node: NodeIdx) -> usize {
        self.graph.adj_lists[node.0]
            .iter()
            .map(|(_other_node, edge)| if self.discovered_edges[edge.0] { 0 } else { 1 })
            .sum()
    }

    fn explore_todo_start_infections(&mut self) {
        // TODO: Short circuit if all edges are discovered
        // println!("Exploring todo start infections");
        // println!("\tQueue: {:?}", self.todo_start_infections);

        while !self.todo_start_infections.is_empty() {
            let (node, time) = self.todo_start_infections.pop().unwrap();
            // println!("\tStarting infection at {:?} at {:?}", node, time);
            if (self.count_missing_edges_at_node(node) > 0)
                || self.args.dont_skip_redundant_start_infections
            {
                self.simulate_infection(node, time);
                self.past_start_infections[node.0].push(time);
            }
        }
    }

    fn number_of_restarts(&self) -> usize {
        let mut sum = 0;
        for node in 0..self.graph.node_count() {
            sum += self.past_start_infections[node].len()
        }
        sum
    }
}

#[derive(Debug, Serialize)]
struct ExperimentResult {
    probability: f64,
    node_count: usize,
    edge_count: usize,
    restarts: usize,
    restarts_for_component_discovery: usize,
    tmax: usize,
}

fn write_results(results: &Vec<ExperimentResult>) -> Result<(), Box<dyn Error>> {
    fs::create_dir_all("./data")?;
    let now = SystemTime::now();
    let now_utc: DateTime<Utc> = now.into();
    let file = File::create(format!(
        "./data/experiment-results-{}.csv",
        now_utc.to_rfc3339()
    ))?;
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

#[derive(Debug, Copy, Clone)]
struct ExperimentTask {
    nodes: usize,
    probability: f64,
}

/// Simple program to greet a person
#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
struct Args {
    /// If a node is marked for a start infection, but all adjacent edges are discovered, don't skip
    #[arg(short, long)]
    dont_skip_redundant_start_infections: bool,
}

fn main() {
    let iterations = MAX_NODES * REPEATS * PROBABILITIES.len();
    let bar = ProgressBar::new(iterations as u64);

    let mut tasks = Vec::with_capacity(iterations);

    for probability in PROBABILITIES {
        for nodes in 1..=MAX_NODES {
            for _repeat in 1..=REPEATS {
                tasks.push(ExperimentTask { probability, nodes });
            }
        }
    }

    let args = Args::parse();

    let results: Vec<ExperimentResult> = tasks
        .par_iter()
        .progress_with(bar)
        .map(move |task: &ExperimentTask| {
            let graph =
                TemporalGraph::gen_erdos_renyi(task.nodes, Time(task.nodes), task.probability);
            let mut execution = FollowAlgorithmExecution::new(graph, args.clone());
            execution.execute();
            ExperimentResult {
                probability: task.probability,
                node_count: execution.graph.node_count(),
                edge_count: execution.graph.edge_count(),
                restarts: execution.number_of_restarts(),
                restarts_for_component_discovery: execution.restarts_for_component_discovery,
                tmax: task.nodes,
            }
        })
        .collect();

    write_results(&results).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;
    use more_asserts::*;

    fn generate_example2() -> TemporalGraph {
        TemporalGraph::from_edge_list(
            vec![
                (NodeIdx(0), NodeIdx(1), Time(4)),
                (NodeIdx(1), NodeIdx(2), Time(5)),
            ],
            Time(10),
        )
    }

    #[test]
    fn simulate_infection_example1() {
        let graph =
            TemporalGraph::from_edge_list(vec![(NodeIdx(0), NodeIdx(1), Time(4))], Time(10));
        let mut execution = FollowAlgorithmExecution::new(
            graph,
            Args {
                dont_skip_redundant_start_infections: false,
            },
        );
        execution.simulate_infection(NodeIdx(0), Time(4));
        assert!(execution.infection_attempts[0][5]);
        assert!(!execution.infection_attempts[0][4]);
    }

    #[test]
    fn simulate_infection_example2() {
        let mut execution = FollowAlgorithmExecution::new(
            generate_example2(),
            Args {
                dont_skip_redundant_start_infections: false,
            },
        );
        execution.simulate_infection(NodeIdx(0), Time(3));
        println!("{:?}", execution.infection_attempts);
        assert!(!execution.infection_attempts[0][3]);
        assert!(execution.infection_attempts[0][4]);
        assert!(execution.infection_attempts[1][5]);
        assert!(execution.infection_attempts[0][5]);
        assert_eq!(execution.discovered_edges, vec![true, true]);
        assert_eq!(execution.discovered_edges_count, 2);
    }

    fn assert_all_edges_discovered(execution: &FollowAlgorithmExecution) {
        assert_eq!(
            execution.discovered_edges_count,
            execution.graph.edge_count()
        );
        for (edge_idx, (left, right, time)) in execution.graph.edges.iter().enumerate() {
            assert!(execution.discovered_edges[edge_idx]);
        }
    }

    fn execute_follow_skipped_unskipped(
        graph: TemporalGraph,
    ) -> (FollowAlgorithmExecution, FollowAlgorithmExecution) {
        let mut execution_skipped = FollowAlgorithmExecution::new(
            graph.clone(),
            Args {
                dont_skip_redundant_start_infections: false,
            },
        );
        let mut execution_unskipped = FollowAlgorithmExecution::new(
            graph,
            Args {
                dont_skip_redundant_start_infections: true,
            },
        );
        execution_skipped.execute();
        execution_unskipped.execute();

        assert_all_edges_discovered(&execution_skipped);
        assert_all_edges_discovered(&execution_unskipped);

        (execution_skipped, execution_unskipped)
    }

    #[test]
    fn execute_follow_example2() {
        let (execution_skipped, execution_unskipped) =
            execute_follow_skipped_unskipped(generate_example2());

        assert_eq!(execution_skipped.restarts_for_component_discovery, 4);
        assert_eq!(execution_unskipped.restarts_for_component_discovery, 4);
    }

    #[test]
    fn test_skipping_helps() {
        let (execution_skipped, execution_unskipped) =
            execute_follow_skipped_unskipped(TemporalGraph::gen_erdos_renyi(100, Time(100), 0.5));
        assert_le!()
    }
}

// General TODOS
// - [ ] Write tests
// - [ ] Better Experimental Setup
// - [ ] Write logging with sensible levels
// - [ ] Add CLI arguments
// - [ ] Write export to file
// - [ ] Include experimental metadata in files
