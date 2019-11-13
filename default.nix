{ ... }@args:
with import <nixpkgs> ({
  overlays = [
    # the overlay(s) here to set default Haskell package set
    # (i.e. `haskellPackages`), where `haskellPackages.ghc`
    # becomes the default Haskell compiler
    (import ./nixpkgs-overlays/ghc865ife-overlay.nix)
  ];
} // args);

#
# export the whole package set here, to install Hadui by Nix:
# 
# nix-env -iA hadui -f https://github.com/complyue/hadui/archive/stable.tar.gz
# 
haskellPackages.extend (haskell.lib.packageSourceOverrides { hadui = ./hadui; })
