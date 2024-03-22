{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = {
    self,
    flake-utils,
    opam-nix,
    nixpkgs,
  }: let
    makeOpamPkgs = {
      devPackages,
      ocamlVersion,
      overlay ? (final: prev: {}),
    }: system: let
      pkgs = import nixpkgs {
        inherit system;
      };

      opamNixLib = opam-nix.lib.${system};

      localPackagesQuery =
        builtins.mapAttrs (_: pkgs.lib.last)
        (opamNixLib.listRepo (opamNixLib.makeOpamRepo ./.));

      devPackagesQuery = devPackages;

      query =
        devPackagesQuery
        // {ocaml-base-compiler = ocamlVersion;};

      scope = (opamNixLib.buildOpamProject' {} ./. query).overrideScope overlay;

      devPackages' =
        builtins.attrValues
        (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope);

      packages = pkgs.lib.getAttrs (builtins.attrNames localPackagesQuery) scope;
    in {
      legacyPackages = scope;
      devPackages = devPackages';
      inherit packages;
    };

    perSystem = makeOpamPkgs {
      ocamlVersion = "5.1.1";
      devPackages = {
        ocaml-lsp-server = "*";
        ocamlformat = "*";
        utop = "*";
      };
    };
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
      };

      p = perSystem system;
    in {
      legacyPackages = p.legacyPackages;

      packages = p.packages;

      devShells.default = pkgs.mkShell {
        inputsFrom = builtins.attrValues p.packages;
        buildInputs =
          p.devPackages
          ++ pkgs.lib.optional (builtins.elem system pkgs.xtrace.meta.platforms) pkgs.xtrace;
      };
    });
}
