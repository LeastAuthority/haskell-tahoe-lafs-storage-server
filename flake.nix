{
  description = "haskell-tahoe-lafs-storage-server";

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;

    statvfs = {
      url = github:shapr/statvfs;
      flake = false;
    };

    flake-utils.url = github:numtide/flake-utils;
    pre-commit-hooks.url = github:cachix/pre-commit-hooks.nix;
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    pre-commit-hooks,
    flake-utils,
    statvfs,
  }: let
    utils = flake-utils.lib;
  in
    utils.eachDefaultSystem (system: let
      compilerVersion = "ghc8107";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          haskell-tahoe-lafs-storage-server = hfinal.callCabal2nix "haskell-tahoe-lafs-storage-server" ./. {};
          servant-pandoc = hfinal.callCabal2nix "servant-pandoc" servant-pandoc {};
          servant-py = hfinal.callCabal2nix "servant-py" servant-py {};
          statvfs = hfinal.callCabal2nix "statvfs" statvfs {};
        };
      };
    in rec {
      packages =
        utils.flattenTree
        {haskell-tahoe-lafs-storage-server = hsPkgs.haskell-tahoe-lafs-storage-server;};

      # nix flake check
      checks = {
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            alejandra.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
          };
        };
      };

      # nix develop
      devShell = hsPkgs.shellFor {
        inherit (self.checks.${system}.pre-commit-check) shellHook;
        withHoogle = true;
        packages = p: [
          p.haskell-tahoe-lafs-storage-server
        ];
        buildInputs = with pkgs;
          [
            hsPkgs.haskell-language-server
            haskellPackages.cabal-install
            cabal2nix
            haskellPackages.hasktags
            haskellPackages.ghcid
            haskellPackages.fourmolu
            haskellPackages.ormolu
            haskellPackages.cabal-fmt
            nodePackages.serve
          ]
          ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
      };

      # nix build
      defaultPackage = packages.haskell-tahoe-lafs-storage-server;
    });
}
