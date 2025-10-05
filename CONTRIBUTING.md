## Development environment

### Nix

This repository has a nix flake that has everything you will need to run the tests.
You can just use `nix develop` or [`direnv`](https://direnv.net/) with the provided `.envrc`.
Notice that it uses a newer version of Scryer Prolog from the git repo than the one provided by
nixpkgs, so you may need to compile it, which may take some time. You can use the same binary
cache as the CI with `cachix use pkg-pl` to get the precompiled environment.

### Non-Nix

You will need a [Scryer Prolog](https://github.com/mthom/scryer-prolog) built close to `master`
(`0.9.4`, the stable release, is too old for what we are doing here),
[just](https://github.com/casey/just) and git.

## Running the tests

You can run `just test` in the root directory and it will run all the tests, same as CI. You can
also run `just ci` to run the entire CI.

## Writing tests

### Snapshot tests

The snapshot tests are in the `tests/snapshot/` directory. The idea is that we run an arbitrary
script and check that everything it does what we expect.

A test case is composed of the following files in the `tests/snapshot/cases/` directory:

- The test script `test_name.sh` that runs the test.
- (Optional) A `test_name.stdin` file whose contents will be passed to the test script in stdin.
- (Optional) A `test_name.in` directory where the script will be run. Can be used to provide files
  for the test to operate on.
- (Can be generated) A `test_name.stdout` with the expected stdout of the test.
- (Can be generated) A `test_name.stderr` with the expected stderr of the test.
- (Can be generated) A `test_name.status` with the expected exit status code of the test.

The last 3 files can be generated from the current behavior of the test with `just snapshot`.

In the `tests/snapshot/` directory, you can use the following just commands to deal with snapshot
tests:

- `just test` actually runs and checks the tests.
- `just dump` runs the tests and puts the generated files in a `dump/` directory for inspection
  without checking them. Useful for manual inspection.
- `just snapshot` runs the tests and puts the generated files in the `cases/` directory.
  Useful if you are pretty sure the current behavior is correct.

