{
  description = "a Nix flake library for Haskell projects";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.url = github:cachix/pre-commit-hooks.nix;
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
  }: {
    lib = import ./lib.nix {
      inherit self nixpkgs pre-commit-hooks flake-utils;
    };
  };
}
