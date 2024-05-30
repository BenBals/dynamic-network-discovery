use std::fs::File;
use std::io;
use std::io::ErrorKind::UnexpectedEof;
use std::ops::AddAssign;
use std::path::Path;
use derive_more::Add;
use rand::{Rng, RngCore, thread_rng};
use rand::prelude::StdRng;
use crate::util::vec_has_duplicates;

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

}
