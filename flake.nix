{
  description = "Cardano Node";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # Would rather use iohkNix/nixpkgs here but we may need to add a flake there first
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            # This overlay adds the cardano-node project to pkgs
            cardanoNodeProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8104";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.cardanoNodeProject.flake {};
      in flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."cardano-node:exe:cardano-node";

        # This is used by `nix develop .` to open a devShell
        devShell = pkgs.cardanoNodeProject.shellFor {
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };
      }
    );
}
