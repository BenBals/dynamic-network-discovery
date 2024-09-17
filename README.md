# Dynamic Network Discovery via Infection Tracing (Experiments)
This is the code for the experiments for our work on "Dynamic Network Discovery via Infection Tracing". You can use it to replicate our results or reuse the code for other projects on spreading processes in temporal graphs.

## Why might this be helpful to me?
While this code was written to test a specific algorithm for network discovery we propose in an upcoming publication, it includes a full implementation of the SIR model on temporal graphs which you can use to simulate infection behavior. This implementation can function as a starting points for a variety of experiments in the same model, regardless of whether they are connected to network discovery. 

## What Graphs Can I Run This On?
Our program supports both temporal Erdős-Renyi graphs which it generates itself or graphs provided by external files. In particular, we support the data format of the SNAP `comm-f2f-Resistance` dataset available at <https://snap.stanford.edu/data/comm-f2f-Resistance.html>. Per default, we generate temporal Erdős-Renyi graphs, but you can use the `--from-snap-resistance-files` flag to provide a path to the folder with the dataset which will then be used instead.

## How Do I use the Program?
Run the program from your command line and then find the experimental results (e.g., number of rounds it took for each graph to be discovered) in the `data` folder. See the `logs` folder for detailed logs. Refer to the `--help` text for the arguments you can pass.

```
$ target/release/master-thesis-experiments --help
Simulate Infections on Temporal Graphs

Usage: master-thesis-experiments [OPTIONS]

Options:
  -d, --dont-skip-redundant-start-infections
          If a node is marked for a start infection, but all adjacent edges are discovered, don't skip
  -s, --seed <SEED>
          [default: 42]
      --from-snap-resistance-files <FROM_SNAP_RESISTANCE_FILES>...

  -h, --help
          Print help
  -V, --version
          Print version

```

## How Do I Compile This?
You can build this project directly with [Cargo](https://doc.rust-lang.org/cargo/index.html). Install an up-to-date rust toolchain (use [rustup](https://rustup.rs) if you don't know where to start) and then building is as easy as running `cargo build --release`.

## Where Can I Find the Paper?
The paper where you can read about the results obtained from these experiments will soon be 

## Who Can I Reach out to If I Have a Question?
Please email Ben. You can find his contact info at <https://beb.ninja/about>
