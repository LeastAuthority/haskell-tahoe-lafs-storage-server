# Define a library of functionality for constructing Flakes.
{ pre-commit-hooks, flake-utils, ... }:
{ pkgs
# ^ A nixpkgs to use.

, src
# ^ The source of the Haskell library/project to customize the library for.

, compilerVersion
# ^ A string giving the version of ghc and related packages to use.

, packageName
  # ^ A string giving the name of the project/package being packaged.
}:
let
  hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
    overrides = hfinal: hprev: {
      ${packageName} = hfinal.callCabal2nix packageName src {};
    };
  };
in
rec {
  checks = { }: {
    pre-commit-check = preCommitCheck { };
  };

  preCommitCheck = { }:
    pre-commit-hooks.lib.${pkgs.system}.run {
      inherit src;
      hooks = {
        alejandra.enable = true;
        fourmolu.enable = true;
        cabal-fmt.enable = true;
      };
    };

  devShells = {
    # A function that accepts a package set and returns a list of additional
    # buildInputs to include in the dev shell.
    extraBuildInputs ? (pkgs: [])
  }: {
    default = hsPkgs.shellFor {
      inherit (preCommitCheck { }) shellHook;

      # Packages to create this development shell for.  These are usually your
      # local packages.
      packages = p: [ ];

      # Extra packages to make available in the shell.
      buildInputs = with pkgs;
        [
          cabal2nix
          # haskellPackages.cabal-fmt
          haskellPackages.cabal-install
          # haskellPackages.fourmolu
          haskellPackages.ghcid
          hsPkgs.haskell-language-server
          # nodePackages.serve
        ] ++ (extraBuildInputs pkgs);
    };
  };

  packages = { }: flake-utils.lib.flattenTree rec {
    default = hsPkgs.${packageName};
    ${packageName} = default;
  };

  apps = {
    # Create a program that runs hlint.
    hlint = {
      # An argv to pass to hlint in addition to any arguments passed on the
      # `nix run` command line.
      argv ? [ "src/" "test/" ]
    }: {
      type = "app";
      program =
        let
          args = builtins.concatStringsSep " " argv;
        in
          "${pkgs.writeScript "hlint"
            ''
            ${hsPkgs.hlint}/bin/hlint ${args} "$@"
            ''}";
    };
  };

  # haskellDevShell = { pkgs, system, haskellPackageName, src }:
  #   let
  #     hsPkgs' = hsPkgs {
  #       inherit pkgs compilerVersion haskellPackageName src;
  #     };
  #   in hsPkgs'.shellFor {
  #     inherit (preCommitCheck { inherit system src; }) shellHook;
  #     withHoogle = true;
  #     packages = p: [
  #       p.${haskellPackageName} # tahoe-lafs-immutable-uploader
  #     ];
  #     buildInputs = with pkgs;
  #       [
  #         haskellPackages.cabal-fmt
  #         haskellPackages.fourmolu
  #         haskellPackages.ghcid
  #         hsPkgs.haskell-language-server
  #         nodePackages.serve
  #       ]
  #       ++ (builtins.attrValues (import ./scripts.nix {
  #         s = pkgs.writeShellScriptBin;
  #       }));
  #   };

}
