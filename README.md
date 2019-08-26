# kobe

Constraint benchmarking tools suite for various solvers including GeCode, Chuffed and AbSolute.

This project is divided into three executables: `kobegen`, `kobe` and `kobeview`.

## Example

## Kobe tools suite

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
