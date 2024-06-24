use more_asserts::assert_ge;
use rand::prelude::StdRng;
use rand::{thread_rng, Rng, RngCore, SeedableRng};
use std::collections::HashSet;
use std::fs::File;
use std::io;
use std::io::BufRead;
use std::io::ErrorKind::UnexpectedEof;
use std::path::Path;

use super::common::*;

impl TemporalGraph {
    fn gen_erdos_renyi(size: usize, tmax: Time, delta: Time, probability: f64) -> TemporalGraph {
        let mut rng: Box<dyn RngCore> = Box::new(thread_rng());
        Self::gen_erdos_renyi_with_rng(size, tmax, delta, probability, &mut rng)
    }

    pub fn gen_erdos_renyi_from_seed(
        size: usize,
        tmax: Time,
        delta: Time,
        probability: f64,
        seed: u64,
    ) -> TemporalGraph {
        let mut rng: Box<dyn RngCore> = Box::new(StdRng::seed_from_u64(seed));
        Self::gen_erdos_renyi_with_rng(size, tmax, delta, probability, &mut rng)
    }

    pub fn gen_erdos_renyi_with_rng(
        size: usize,
        tmax: Time,
        delta: Time,
        probability: f64,
        rng: &mut Box<dyn RngCore>,
    ) -> TemporalGraph {
        let mut adj_lists = vec![vec![]; size];

        assert_ge!(tmax.0, 1);

        let mut edges =
            Vec::with_capacity((probability * size as f64 * size as f64).ceil() as usize);

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
            delta,
        }
    }

    /// Parse a contact network from the SNAP dataset
    /// Note that because the snap dataset is directed, we add an edge a <-> b where a < b iff there
    /// is an edge a <- b in the dataset
    pub(crate) fn from_snap_resistance_csv<P>(path: P) -> io::Result<TemporalGraph>
    where
        P: AsRef<Path>,
    {
        let mut edge_list: Vec<(NodeIdx, NodeIdx, Time)> = Vec::new();
        let file = File::open(path)?;
        let mut lines = io::BufReader::new(file).lines().flatten();

        let first_line = lines.next().ok_or(io::Error::new(
            UnexpectedEof,
            "SNAP graph csv file did not have header line",
        ));

        // let n: usize =
        //    f64::round(f64::sqrt( + 0.25) + 0.5)
        //        as usize;
        let adj_matrix_length = first_line?.split(",").count() - 1;
        let n = f64::round(0.5 * (f64::sqrt(4.0 * adj_matrix_length as f64 + 1.0) + 1.0)) as usize;
        assert_eq!(adj_matrix_length, n * (n - 1));

        let mut time: usize = 1;
        for line in lines {
            let adj_bits = line.split(",").skip(1).map(|str| str == "1").enumerate();
            for (index, bit) in adj_bits {
                if bit {
                    let from = index / n + 1;
                    let to = index % n;

                    if to < from {
                        edge_list.push((NodeIdx(to), NodeIdx(from), Time(time)));
                    }
                }
            }
            time += 1;
        }

        let tmax = edge_list
            .iter()
            .map(|(_, _, time)| time.0)
            .max()
            .unwrap_or(0)
            + 1;

        // Check that the edge list is unique
        let mut edge_set = HashSet::new();
        for edge in &edge_list {
            assert!(
                edge_set.insert(edge.clone()),
                "duplicate edge: {:?} (n = {})",
                edge,
                n
            )
        }

        Ok(TemporalGraph::from_edge_list(edge_list, Time(tmax), Time(1)))
    }
}
