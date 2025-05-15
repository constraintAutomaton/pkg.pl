# Development environment

## Nix

This repository has a nix flake that has everything you will need to run the tests.
You can just use `nix develop` or [`direnv`](https://direnv.net/) with the provided `.envrc`.
Notice that it uses a newer version of Scryer Prolog from the git repo than the one provided by
nixpkgs, so you may need to compile it, which may take some time. You can use the same binary
cache as the CI with `cachix use pkg-pl` to get the precompiled environment.

## Non-Nix

You will need a [Scryer Prolog](https://github.com/mthom/scryer-prolog) built close to `master`
(`0.9.4`, the stable release, is too old for what we are doing here),
[just](https://github.com/casey/just) and git.

# Running the tests

You can run `just test` in the root directory and it will run all the tests, same as CI.
