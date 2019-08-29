# Constraint benchmarking tools suite (kobe)

Constraint benchmarking tools suite for various solvers including GeCode, Chuffed and AbSolute.

The goals of this benchmarking tools suite are:

1. Replicability of the research results.
2. Report of the full results whereas in research papers it is often summarized.
3. Automate benchmarking of different solvers on different problems.

This work is on-going and replicability problems might still be present.
In case of problems, please do not hesitate to contact us on the [issues tracker](https://github.com/ptal/kobe/issues) or by [email](mailto:pierre.talbot@univ-nantes.fr).

## Kobe tools suite

This project is divided into three executables: `kobegen`, `kobe` and `kobeview`.
The three executables rely on a common library named `kobecore` containing mostly the format of the benchmarking configuration files, and file manipulation utilities.

### Kobe benchmarks generator (`kobegen`)

From a general benchmark specification file, `kobegen` will generate `n` files for each benchmark instance to test.
Such file can be given to `kobe` in order to perform the benchmark.

### Constraint benchmarking (`kobe`)

This project measures how fast a configuration of a solver is on a set of problem instances.

### Kobe data viewer (`kobeview`).

This project is useful to analyze benchmark results collected with `kobe`.
One goal is to compare different solver to point out advantages and weaknesses.
For example, we would like to know which problem's instances AbSolute can solve that GeCode cannot, or if we could improve a known lower bound on some problems.

### Contributors

This project was initiated and is maintained by Pierre Talbot.
Tom Perroux also contributed to `kobeview` during its internship, at this time the project was located in the [AbSolute solver](https://github.com/ptal/AbSolute).
