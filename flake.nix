{
  description = "generate sentences in context-sensitive languages";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          gnoam =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8105";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                ghcid = {};
                ormolu = {};
                hlint = {};
                hpack = {};
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.gnoam.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages.gnoam;
    });
}
