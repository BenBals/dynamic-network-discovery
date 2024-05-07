use std::ops::AddAssign;
use clap::Parser;
use rand::{thread_rng, Rng};
use serde::Serialize;
use csv;
use std::{error::Error, fs};
use std::fs::File;
use chrono::{DateTime, Utc}; // 0.4.15
use std::time::SystemTime;
use indicatif::ProgressBar;


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct NodeIdx(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct EdgeIdx(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Time(usize);

#[derive(Debug)]
struct TemporalGraph {
    adj_lists: Vec<Vec<(NodeIdx, EdgeIdx)>>,
    edges: Vec<(NodeIdx, NodeIdx, Time)>,
    tmax: usize,
}

impl AddAssign<usize> for Time {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs
    }
}

impl TemporalGraph {
    fn gen_erdos_renyi(size: usize, tmax: usize, probability: f64) -> TemporalGraph {
        let mut adj_lists = vec![vec![]; size];

        let mut edges =
            Vec::with_capacity((probability * size as f64 * size as f64).ceil() as usize);
        let mut rng = thread_rng();

        for i in 0..size {
            for j in 0..i {
                if rng.gen_bool(probability) {
                    edges.push((NodeIdx(j), NodeIdx(i), Time(rng.gen_range(0..tmax))));

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
}

struct FollowAlgorithmExecution {
    graph: TemporalGraph,
    past_start_infections: Vec<Vec<Time>>,
    todo_start_infections: Vec<(NodeIdx, Time)>,
    discovered_edges: Vec<bool>,
    infection_attempts: Vec<Vec<bool>>,
    discovered_edges_count: usize,
}

impl FollowAlgorithmExecution {
    fn new(graph: TemporalGraph) -> FollowAlgorithmExecution {
        Self {
            past_start_infections: vec![vec![]; graph.node_count()],
            discovered_edges: vec![false; graph.edge_count()],
            discovered_edges_count: 0,
            infection_attempts: vec![vec![false; graph.tmax]; graph.edge_count()],
            todo_start_infections: vec![],
            graph,
        }
    }

    fn is_immune(&self, node: NodeIdx, infection_log: &Vec<Option<Time>>) -> bool {
        infection_log[node.0].is_some()
    }

    fn make_infection_attempt(&mut self, edge: (NodeIdx, EdgeIdx), time: Time, infection_log: &mut Vec<Option<Time>>) -> bool {
        if (self.graph.edge_label(edge.1) == time) && !self.is_immune(edge.0, infection_log) {
            self.discovered_edges[edge.1.0] = true;
            self.discovered_edges_count += 1;
            infection_log[edge.0.0] = Some(time);
            return true;
        }
        if !self.is_immune(edge.0, infection_log) {
            self.infection_attempts[edge.1.0][time.0] = true
        }
        false
    }

    fn simulate_infection(&mut self, root_node: NodeIdx, infection_time: Time) -> (Vec<Option<Time>>, Vec<EdgeIdx>) {
        let mut infected: Vec<NodeIdx> = vec![root_node];
        let mut time = infection_time;
        let mut infection_log = vec![None; self.graph.node_count()];
        let mut infected_edges = vec![];

        while !infected.is_empty() & (time.0 < self.graph.tmax) {
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
                return self.find_new_edge_starting_at_node(self.graph.edges[edge].0).unwrap();
            }
        }

        panic!("This should never happen")
    }

    fn find_new_edge_starting_at_node(&mut self, start_node: NodeIdx) -> Option<EdgeIdx> {
        // TODO: Optimize the clone away
        for neighbour in self.graph.neighbours(start_node).clone() {
            // println!("\tAttempting to find edge between {:?} and {:?}", start_node, neighbour);
            // println!("\t\tPreviously discovered: {:?}", self.discovered_edges);
            if !self.discovered_edges[neighbour.1.0] {
                for time in 0..self.graph.tmax {
                    // println!("\t\tTesting time {}", time);
                    if !self.infection_attempts[neighbour.1.0][time] {
                        self.past_start_infections[neighbour.0.0].push(Time(time));
                        let (infection_log, infected_edges) = self.simulate_infection(start_node, Time(time));
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

    fn explore_todo_start_infections(&mut self) {
        // TODO: Short circuit if all edges are discovered
        // println!("Exploring todo start infections");
        // println!("\tQueue: {:?}", self.todo_start_infections);

        while !self.todo_start_infections.is_empty() {
            let (node, time) = self.todo_start_infections.pop().unwrap();
            // println!("\tStarting infection at {:?} at {:?}", node, time);
            self.simulate_infection(node, time);
            self.past_start_infections[node.0].push(time);
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
}

fn write_results(results: &Vec<ExperimentResult>) -> Result<(), Box<dyn Error>> {
    fs::create_dir_all("./data")?;
    let now = SystemTime::now();
    let now_utc: DateTime<Utc> = now.into();
    let file = File::create(format!("./data/experiment-results-{}.csv", now_utc.to_rfc3339()))?;
    let mut wtr = csv::Writer::from_writer(file);

    for result in results {
        wtr.serialize(result)?;
    }

    wtr.flush()?;
    Ok(())
}

const MAX_NODES: usize = 1000;
const REPEATS: usize = 10;
const PROBABILITIES: [f64; 5] = [0.01, 0.05, 0.1, 0.3, 0.5];

fn main() {
    let mut results = vec![];

    let iterations = MAX_NODES * REPEATS * PROBABILITIES.len();
    let bar = ProgressBar::new(iterations as u64);

    for probability in PROBABILITIES {
        for count in 1..=MAX_NODES {
            for repeat in 1..=REPEATS {
                let graph = TemporalGraph::gen_erdos_renyi(count, count, probability);
                let mut execution = FollowAlgorithmExecution::new(graph);
                execution.execute();
                results.push(ExperimentResult {
                    probability,
                    node_count: execution.graph.node_count(),
                    edge_count: execution.graph.edge_count(),
                    restarts: execution.number_of_restarts(),
                });
                bar.inc(1);
            }
        }
    }

    bar.finish();

    write_results(&results).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn todo() {
        todo!()
        // Assert that at the end, always all edges are known
    }
}

// General TODOS
// - [ ] Write tests
// - [ ] Better Experimental Setup
// - [ ] Write logging with sensible levels
// - [ ] Add CLI arguments
// - [ ] Write export to file
// - [ ] Include experimental meta data in files