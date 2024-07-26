{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              self.overlays.default
            ];
          };
          hl = pkgs.haskell.lib;
        in
        {
          packages.yesod-subsite-radio-field = pkgs.haskellPackages.yesod-subsite-radio-field;
          packages.default = pkgs.lib.trivial.pipe pkgs.haskellPackages.yesod-subsite-radio-field
            [
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];

          checks = {
            inherit (pkgs.haskellPackages) yesod-subsite-radio-field;
          };

          devShells.default = pkgs.haskellPackages.shellFor {
            packages = p: [ p.yesod-subsite-radio-field ];
            buildInputs = with pkgs.haskellPackages; [
              cabal-fmt
              cabal-install
              hlint
            ];
          };
        }) // {
      overlays.default = _: prev: {
        haskell = prev.haskell // {
          # override for all compilers
          packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides (_: hprev: {
            yesod-subsite-radio-field =
              let
                haskellSourceFilter = prev.lib.sourceFilesBySuffices ./. [
                  ".cabal"
                  ".hs"
                ];
              in
              hprev.callCabal2nix "yesod-subsite-radio-field" haskellSourceFilter { };
          });
        };
      };
    };
}
