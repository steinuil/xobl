{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = {
    self,
    flake-utils,
    opam-nix,
    nixpkgs,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      on = opam-nix.lib.${system};
      localPackagesQuery =
        builtins.mapAttrs (_: pkgs.lib.last)
        (on.listRepo (on.makeOpamRepo ./.));
      devPackagesQuery = {
        ocaml-lsp-server = "*";
        ocamlformat = "*";
        utop = "*";
      };
      query =
        devPackagesQuery
        // {
          ## You can force versions of certain packages here, e.g:
          ## - force the ocaml compiler to be taken from opam-repository:
          ocaml-base-compiler = "5.1.1";
          ## - or force the compiler to be taken from nixpkgs and be a certain version:
          # ocaml-system = "4.14.0";
          ## - or force ocamlfind to be a certain version:
          # ocamlfind = "1.9.2";
        };
      scope = on.buildOpamProject' {} ./. query;
      overlay = final: prev: {
        # You can add overrides here
      };
      scope' = scope.overrideScope' overlay;
      # Packages from devPackagesQuery
      devPackages =
        builtins.attrValues
        (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
      # Packages in this workspace
      packages =
        pkgs.lib.getAttrs (builtins.attrNames localPackagesQuery) scope';
    in {
      legacyPackages = scope';

      inherit packages;

      devShells.default = pkgs.mkShell {
        inputsFrom = builtins.attrValues packages;
        buildInputs =
          devPackages
          ++ (
            if builtins.elem system pkgs.xtrace.meta.platforms
            then [pkgs.xtrace]
            else []
          );
      };
    });
}
