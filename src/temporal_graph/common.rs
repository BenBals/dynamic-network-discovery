use crate::util::vec_has_duplicates;
use derive_more::Add;
use ena::unify::{EqUnifyValue, InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};
use std::collections::HashMap;
use std::ops::AddAssign;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub(crate) struct NodeIdx(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct EdgeIdx(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Add, Hash)]
pub(crate) struct Time(pub usize);

#[derive(Debug, Clone)]
pub(crate) struct TemporalGraph {
    /// Vector indexed by `NodeIdx` where each node has a list of it's neighbours and the edge
    /// connecting them
    pub adj_lists: Vec<Vec<(NodeIdx, EdgeIdx)>>,
    /// An edge is represented by its two adjacent nodes (ordered by index) and its time label
    /// Also note that there may not be two identical edges (same nodes *and* time).
    pub edges: Vec<(NodeIdx, NodeIdx, Time)>,
    /// Edges may be labels with labels in `0..tmax.0`. NB, this doesn't include `tmax` itself.
    pub tmax: Time,
}

impl AddAssign<usize> for Time {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs
    }
}

impl TemporalGraph {
    pub fn node_count(&self) -> usize {
        self.adj_lists.len()
    }

    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }

    pub fn edge_label(&self, edge: EdgeIdx) -> Time {
        self.edges[edge.0].2
    }

    pub fn neighbours(&self, node: NodeIdx) -> &Vec<(NodeIdx, EdgeIdx)> {
        &self.adj_lists[node.0]
    }

    pub fn is_edge_adjacent_to_node(&self, edge: EdgeIdx, node: NodeIdx) -> bool {
        (self.edges[edge.0].0 == node) | (self.edges[edge.0].1 == node)
    }

    pub fn are_two_edges_adjacent(&self, e1: EdgeIdx, e2: EdgeIdx) -> bool {
        let (u, v, _) = self.edges[e1.0];
        let (w, z, _) = self.edges[e2.0];
        u == w || u == z || v == w || v == z
    }

    pub fn from_edge_list(edges: Vec<(NodeIdx, NodeIdx, Time)>, tmax: Time) -> TemporalGraph {
        assert!(!vec_has_duplicates(edges.clone()));

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

    pub fn delta_egde_connected_components(&self) -> Vec<Vec<EdgeIdx>> {
        let mut union_find: InPlaceUnificationTable<EdgeUnifyKey> = InPlaceUnificationTable::new();
        union_find.reserve(self.edge_count());

        let mut sorted_edges: Vec<(Time, NodeIdx, NodeIdx, EdgeIdx)> = self
            .edges
            .iter()
            .enumerate()
            .map(|(edge_idx, (u, v, t))| (*t, *u, *v, EdgeIdx(edge_idx)))
            .collect();
        sorted_edges.sort();
        let mut node_component_map: Vec<Option<(EdgeUnifyKey, Time)>> =
            vec![None; self.node_count()];

        let mut edge_to_key = Vec::with_capacity(self.edge_count());
        for edge_idx in 0..self.edge_count() {
            edge_to_key.push(union_find.new_key(EdgeIdx(edge_idx)));
        }
        fn relevant_component_at_node(
            node_component_map: &Vec<Option<(EdgeUnifyKey, Time)>>,
            node: NodeIdx,
            time: Time,
        ) -> Option<EdgeUnifyKey> {
            if let Some((key, inner_time)) = node_component_map[node.0] {
                if (inner_time.0 - 1 <= time.0) & (time.0 <= inner_time.0 + 1) {
                    // println!("\tRelevant component at node {:?} at time {:?}", node, time);
                    return Some(key);
                }
            }
            // println!("\tNO relevant component at node {:?} at time {:?}", node, time);
            None
        };

        for (t, u, v, edge_idx) in sorted_edges {
            // println!("Processing edge {:?}", (t, u, v, edge_idx));
            if let (Some(c1), Some(c2)) = (
                relevant_component_at_node(&node_component_map, u, t),
                relevant_component_at_node(&node_component_map, v, t),
            ) {
                // println!("Unioning components of {:?} and {:?}", union_find.probe_value(c1), union_find.probe_value(c2));
                union_find.union(c1, c2);
            }
            for neighbor in [u, v] {
                if let Some(component) =
                    relevant_component_at_node(&node_component_map, neighbor, t)
                {
                    // println!("Unioning components of {:?} and {:?}", union_find.probe_value(component), union_find.probe_value(edge_to_key[edge_idx.0]));
                    union_find.union(component, edge_to_key[edge_idx.0])
                }
                node_component_map[neighbor.0] = Some((edge_to_key[edge_idx.0], t))
            }
        }

        let mut components: HashMap<EdgeUnifyKey, Vec<EdgeIdx>> = HashMap::new();
        for edge_idx in 0..self.edge_count() {
            let key = union_find.find(edge_to_key[edge_idx]);
            components
                .entry(key)
                .or_insert(vec![])
                .push(EdgeIdx(edge_idx));
        }

        components.values().cloned().collect()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Hash, Eq)]
struct EdgeUnifyKey(u32);

impl UnifyValue for EdgeIdx {
    type Error = NoError;

    fn unify_values(value1: &Self, _value2: &Self) -> Result<EdgeIdx, NoError> {
        Ok(*value1)
    }
}

impl UnifyKey for EdgeUnifyKey {
    type Value = EdgeIdx;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "EdgeIdx"
    }
}

mod tests {
    use crate::temporal_graph::common::{EdgeIdx, TemporalGraph, Time};
    use std::process::id;

    fn generate_n25_erdos_renyi_graph() -> TemporalGraph {
        TemporalGraph::gen_erdos_renyi_from_seed(3, Time(25), 0.5, 420)
    }

    /// Note: This only checks that two adjacent edges with similar time label are in the same component
    /// This doesn't make negative checks
    fn assert_connected_components_not_too_fine(graph: &TemporalGraph) {
        // println!("========================================================");
        // println!("{:?}", graph);
        let components = graph.delta_egde_connected_components();
        let mut id_map: Vec<Option<usize>> = vec![None; graph.edge_count()];

        for (cid, component) in components.iter().enumerate() {
            for edge in component {
                id_map[edge.0] = Some(cid);
            }
        }

        assert!(id_map.iter().all(Option::is_some));

        for e1 in 0..graph.edge_count() {
            for e2 in 0..graph.edge_count() {
                if graph.are_two_edges_adjacent(EdgeIdx(e1), EdgeIdx(e2))
                    && graph.edge_label(EdgeIdx(e1)).0 - 1 <= graph.edge_label(EdgeIdx(e2)).0
                    && graph.edge_label(EdgeIdx(e1)).0 + 1 >= graph.edge_label(EdgeIdx(e2)).0
                {
                    assert_eq!(id_map[e1], id_map[e2])
                }
            }
        }
    }
    #[test]
    fn randomized_connected_components_check() {
        let graph1 = generate_n25_erdos_renyi_graph();
        assert_connected_components_not_too_fine(&graph1);

        for size in 0..25 {
            for prob in [0.1, 0.3, 0.9] {
                for tmax in [3, 10, 100] {
                    let graph =
                        TemporalGraph::gen_erdos_renyi_from_seed(size, Time(tmax), prob, 420);
                    assert_connected_components_not_too_fine(&graph);
                }
            }
        }
    }
}
