# This is Nix expression for Hadui the project, exposing
# an overlaid nixpkgs with a new Haskell package set armed
# as the standard (i.e. `haskellPackages`), the package set
# includes Hadui the package (i.e. `haskellPackages.hadui`),
# and Hadui the package is added to top-level as well.
#
# You'd install Hadui the tool with:
# 
# nix-env -iA hadui -f https://github.com/complyue/hadui/archive/0.1.0.0.tar.gz
# 
{ overlays ? [], ... }@args:
let
  haduiOverlay = self: super:
    let
      hpsWithHadui = super.haskellPackages.override {
          overrides = hself: hsuper: {
            hadui = hself.callCabal2nix "hadui" ./hadui {};
          };
      };
    in {
      # the top-level Nix package for Hadui
      hadui = hpsWithHadui.hadui;
      # override the Haskell package set at standard locations
      haskellPackages = hpsWithHadui;
      haskell = super.haskell // {
        packages = super.haskell.packages // { ghcWithHadui = hpsWithHadui; };
      };
    };
in import <nixpkgs> (args // {
  overlays = [
    # this overlay creates & sets-default a new Haskell package
    # set (i.e. `haskellPackages`), with the experimental GHC
    # with interactive frontend support, which is mandatory
    # for Hadui to function.
    (import ./nixpkgs-overlays/ghc865ife-overlay.nix)

    # this overlay as defined above overrides & sets-default
    # a new Haskell package set with Hadui the package (i.e.
    # `haskellPackages.hadui`) included. Hadui the package
    # is also made available at top-level of nixpkgs.
    haduiOverlay
  ] ++ overlays;
})
