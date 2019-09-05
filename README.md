# Constraint benchmarking tools suite (kobe)

[![ptal on Travis CI][travis-image]][travis]

[travis-image]: https://travis-ci.org/ptal/kobe.png?branch=master
[travis]: https://travis-ci.org/ptal/kobe

Constraint benchmarking tools suite for various solvers including GeCode, Chuffed and AbSolute.
Please consult the [benchmarking book](https://ptal.github.io/benchmarking.html) for a tutorial on how to bench and analyse your results.

This project is divided into three executables: `kobegen`, `kobe` and `kobeview`.
The three executables rely on a common library named `kobecore` containing mostly the format of the benchmarking configuration files, and file manipulation utilities.

This work is on-going and replicability problems might still be present.
In case of problems, please do not hesitate to contact us on the [issues tracker](https://github.com/ptal/kobe/issues) or by [email](mailto:pierre.talbot@univ-nantes.fr).

### Contributors

This project was initiated and is maintained by Pierre Talbot.
Tom Perroux also contributed to `kobeview` during its internship, at this time the project was located in the [AbSolute solver](https://github.com/ptal/AbSolute).
