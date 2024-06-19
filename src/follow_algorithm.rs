use crate::temporal_graph::common::*;
use crate::{Args, TemporalGraph};
use log::debug;
use std::cmp::max_by_key;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq)]
enum RestartType {
    Following,
    ComponentDiscovery,
}

pub struct FollowAlgorithmExecution {
    pub graph: TemporalGraph,
    /// `NodeIdx` -> `Vec` of `Time`s where a previous start infection began
    past_start_infections: Vec<HashMap<Time, RestartType>>,
    /// Stack of start infections to perform in the future
    todo_start_infections: Vec<(NodeIdx, Time)>,
    /// `EdgeIdx` -> `Bool`, is `true` if the edge with the respective index has been discovered
    discovered_edges: Vec<bool>,
    /// `EdgeIdx` -> `Time` -> `bool`, is true if there was an infection attempt at the given time
    /// via the edge
    infection_attempts: Vec<Vec<bool>>,
    discovered_edges_count: usize,
    pub restarts_for_component_discovery: usize,
    args: Args,
}

impl FollowAlgorithmExecution {
    pub fn new(graph: TemporalGraph, args: Args) -> FollowAlgorithmExecution {
        Self {
            past_start_infections: vec![HashMap::new(); graph.node_count()],
            discovered_edges: vec![false; graph.edge_count()],
            discovered_edges_count: 0,
            restarts_for_component_discovery: 0,
            infection_attempts: vec![vec![false; graph.tmax.0]; graph.edge_count()],
            todo_start_infections: vec![],
            graph,
            args,
        }
    }

    fn is_immune(&self, node: NodeIdx, infection_log: &Vec<Option<EdgeIdx>>) -> bool {
        infection_log[node.0].is_some()
    }

    fn make_infection_attempt(
        &mut self,
        edge: (NodeIdx, EdgeIdx),
        time: Time,
        infection_log: &mut Vec<Option<EdgeIdx>>,
    ) -> bool {
        let is_immune: bool = self.is_immune(edge.0, infection_log);
        if !is_immune {
            debug!("\tInfection attempt along {:?} at {:?}", edge, time);
            self.infection_attempts[edge.1 .0][time.0] = true;
        }
        if (self.graph.edge_label(edge.1) == time) && !is_immune {
            if !self.discovered_edges[edge.1 .0] {
                self.discovered_edges_count += 1;
                self.discovered_edges[edge.1 .0] = true;
            }
            infection_log[edge.0 .0] = Some(edge.1);
            return true;
        }
        false
    }

    /// Returns
    /// - `infection_log: Vec<Option<EdgeIdx>>`: indexed by `NodeIdx`. Is some edge if the respective
    ///   node has been infected via this edge
    /// - `infected_edges: Vec<EdgeIdx>`
    // TODO: Merge into a single tree structure
    fn simulate_infection(
        &mut self,
        root_node: NodeIdx,
        infection_time: Time,
    ) -> (Vec<Option<EdgeIdx>>, Vec<EdgeIdx>) {
        debug!(
            "Simulating infection at {:?} at {:?}",
            root_node, infection_time
        );
        let mut infected: Vec<NodeIdx> = vec![root_node];
        let mut time = infection_time + Time(1);
        let mut infection_log: Vec<Option<EdgeIdx>> = vec![None; self.graph.node_count()];
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
        let (left, right, label): (NodeIdx, NodeIdx, Time) = self.graph.edges[edge.0];

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
        if !self.is_start_infection_redundant(&node, &time) {
            self.todo_start_infections.push((node, time))
        }
    }

    fn is_start_infection_redundant(&self, node: &NodeIdx, time: &Time) -> bool {
        self.past_start_infections[node.0].contains_key(time)
    }

    fn try_add_past_start_infection(
        &mut self,
        node: NodeIdx,
        time: Time,
        restart_type: RestartType,
    ) {
        if self.is_start_infection_redundant(&node, &time) {
            panic!(
                "Tried to duplicate start infection (node: {:?}, time: {:?}, new type: {:?}, old type: {:?})\n\t{:?}",
                node, time, restart_type, self.past_start_infections[node.0].get(&time),
                self.graph.adj_lists[node.0].iter().map(|(_node, edge)| self.graph.edges[edge.0]).collect::<Vec<_>>(),
            );
        } else {
            self.past_start_infections[node.0].insert(time, restart_type);
        }
    }

    pub fn execute(&mut self) {
        while self.discovered_edges_count < self.graph.edge_count() {
            self.find_start_edge();
            self.explore_todo_start_infections();
        }
    }

    fn find_start_edge(&mut self) -> EdgeIdx {
        for edge in 0..self.graph.edge_count() {
            if self.discovered_edges[edge] == false {
                // println!("Trying to find a start edge at {:?}", self.graph.edges[edge].0);
                return self.find_new_edge(EdgeIdx(edge)).unwrap();
            }
        }

        panic!("This should never happen")
    }

    /// NB, this method not necessarily discovers the label of the targeted edge, but tries to do
    /// so until *some* new edge has been discovered
    /// Panics if the target edge has been discovered before
    fn find_new_edge(&mut self, target_edge: EdgeIdx) -> Option<EdgeIdx> {
        assert!(!self.discovered_edges[target_edge.0]);

        let (left, right, _) = self.graph.edges[target_edge.0];

        let best_candidate = max_by_key(left, right, |node| self.graph.adj_lists[node.0].len());
        debug!(
            "Trying to find label of edge {:?}\n\tPrevious start infections: {:?}\n\tAttempts at target edge: {:?}",
            self.graph.edges[target_edge.0],
            self.past_start_infections.iter(),
            self.infection_attempts[target_edge.0].iter()
        );

        // TODO: Try to optimize clone away
        let previously_discovered_edges = self.discovered_edges.clone();

        for time in 1..self.graph.tmax.0 {
            // Skip if there was in infection attempt along the target edge at the test time
            // TODO: Skip if there is a edge between the same nodes with the same label
            if !self.infection_attempts[target_edge.0][time]
                && !self.is_start_infection_redundant(&best_candidate, &Time(time - 1))
            {
                self.restarts_for_component_discovery += 1;
                self.try_add_past_start_infection(
                    best_candidate,
                    Time(time - 1),
                    RestartType::ComponentDiscovery,
                );
                let (_infection_log, infected_edges) =
                    self.simulate_infection(best_candidate, Time(time - 1));

                // After "testing" the `target_edge` should have an infection attempt at the
                // currently checked time
                assert!(self.infection_attempts[target_edge.0][time]);

                // TODO: If still neccessary, update todo_start_infections here
                for edge in infected_edges {
                    if !previously_discovered_edges[edge.0] {
                        return Some(edge);
                    }
                }
            }
        }

        log::error!(
            "Cloud not find target edge {:?} ({:?})\n\t{:?}",
            target_edge,
            self.graph.edges[target_edge.0],
            self.graph
        );

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
            if (self.count_missing_edges_at_node(node) > 0
                || self.args.dont_skip_redundant_start_infections)
                && !self.is_start_infection_redundant(&node, &time)
            {
                self.try_add_past_start_infection(node, time, RestartType::Following);
                self.simulate_infection(node, time);
            }
        }
    }

    pub fn number_of_restarts(&self) -> usize {
        let mut sum = 0;
        for node in 0..self.graph.node_count() {
            sum += self.past_start_infections[node].len()
        }
        sum
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use more_asserts::*;
    use std::collections::HashSet;
    use std::hash::Hash;

    fn generate_example2() -> TemporalGraph {
        TemporalGraph::from_edge_list(
            vec![
                (NodeIdx(0), NodeIdx(1), Time(4)),
                (NodeIdx(1), NodeIdx(2), Time(5)),
            ],
            Time(10),
        )
    }

    fn generate_example3_multigraph() -> TemporalGraph {
        TemporalGraph::from_edge_list(
            vec![
                (NodeIdx(0), NodeIdx(1), Time(1)),
                (NodeIdx(0), NodeIdx(1), Time(3)),
                (NodeIdx(1), NodeIdx(2), Time(2)),
            ],
            Time(10),
        )
    }

    fn generate_n100_erdos_renyi_graph() -> TemporalGraph {
        TemporalGraph::gen_erdos_renyi_from_seed(100, Time(100), 0.5, 420)
    }

    #[test]
    fn simulate_infection_example1() {
        let graph =
            TemporalGraph::from_edge_list(vec![(NodeIdx(0), NodeIdx(1), Time(4))], Time(10));
        let mut execution = FollowAlgorithmExecution::new(graph, Args::default());
        execution.simulate_infection(NodeIdx(0), Time(4));
        assert!(execution.infection_attempts[0][5]);
        assert!(!execution.infection_attempts[0][4]);
    }

    #[test]
    fn simulate_infection_example2() {
        let mut execution = FollowAlgorithmExecution::new(generate_example2(), Args::default());
        execution.simulate_infection(NodeIdx(0), Time(3));
        assert!(!execution.infection_attempts[0][3]);
        assert!(execution.infection_attempts[0][4]);
        assert!(execution.infection_attempts[1][5]);
        assert!(execution.infection_attempts[0][5]);
        assert_eq!(execution.discovered_edges, vec![true, true]);
        assert_eq!(execution.discovered_edges_count, 2);
    }

    fn execute_erdos_renyi(nodes: usize, tmax: Time, probability: f64) -> FollowAlgorithmExecution {
        let mut execution = FollowAlgorithmExecution::new(
            TemporalGraph::gen_erdos_renyi_from_seed(nodes, tmax, probability, 420),
            Args::default(),
        );

        execution.execute();

        return execution;
    }

    fn assert_all_edges_discovered(execution: &FollowAlgorithmExecution) {
        assert_eq!(
            execution.discovered_edges_count,
            execution.graph.edge_count()
        );
        for (edge_idx, (_left, _right, _time)) in execution.graph.edges.iter().enumerate() {
            assert!(execution.discovered_edges[edge_idx]);
        }
    }

    fn execute_follow_skipped_unskipped(
        graph: TemporalGraph,
    ) -> (FollowAlgorithmExecution, FollowAlgorithmExecution) {
        let mut execution_skipped = FollowAlgorithmExecution::new(graph.clone(), Args::default());
        let mut execution_unskipped =
            FollowAlgorithmExecution::new(graph, Args::erdos_renyi(42, true));
        execution_skipped.execute();
        execution_unskipped.execute();

        assert_all_edges_discovered(&execution_skipped);
        assert_all_edges_discovered(&execution_unskipped);

        (execution_skipped, execution_unskipped)
    }

    fn has_unique_elements<T>(iter: T) -> bool
    where
        T: IntoIterator,
        T::Item: Eq + Hash,
    {
        let mut uniq = HashSet::new();
        iter.into_iter().all(move |x| uniq.insert(x))
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
            execute_follow_skipped_unskipped(generate_n100_erdos_renyi_graph());
        assert_le!(
            execution_skipped.restarts_for_component_discovery,
            execution_unskipped.restarts_for_component_discovery
        )
    }

    #[test]
    fn test_at_most_6m_following_restarts() {
        let execution = execute_erdos_renyi(100, Time(100), 0.2);

        let mut total_follow_restarts = 0;

        for (node_idx, adj_list) in execution.graph.adj_lists.iter().enumerate() {
            let follow_restarts = execution.past_start_infections[node_idx]
                .iter()
                .filter(|restart| *restart.1 == RestartType::Following)
                .count();
            total_follow_restarts += follow_restarts;
            assert_le!(follow_restarts, 3 * adj_list.len());
        }

        assert_le!(total_follow_restarts, 6 * execution.graph.edge_count());
    }

    #[test]
    fn test_correctly_handles_multi_edges() {
        let mut execution =
            FollowAlgorithmExecution::new(generate_example3_multigraph(), Args::default());
        execution.execute();

        assert_all_edges_discovered(&execution);
    }
}
