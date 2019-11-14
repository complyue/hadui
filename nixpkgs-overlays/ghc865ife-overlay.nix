# This is the Nix overlay to arm an experimental branch of GHC
# as the default compiler, together with the full Haskell package
# set (i.e. `haskellPackages`) with it.
#
# The compiler will be compiled from a source distribution, full
# source of the branch is at this repository:
#   https://gitlab.haskell.org/complyue/ghc/tree/ghc-8.6-ife
self: super:
let

  # mock the env at (<nixpkgs> + /pkgs/top-level/haskell-packages)
  callPackage = super.newScope {
    haskellLib = super.haskell.lib;
    overrides = super.pkgs.haskell.packageOverrides;
  };

  # the compiler
  compiler865ife = super.haskell.compiler.ghc865.overrideAttrs (oldAttrs: {
    name = "${oldAttrs.name}-ife";

    src = super.fetchurl {
      name = "ghc-8.6.5-ife-src.tar.xz";
      url =
        "https://gitlab.haskell.org/complyue/ghc-ife-sdist/raw/master/ghc-8.6.5-src.tar.xz";
      sha256 = "0wf5v1ry3rlwhbsxlvka3qscdb4jz4jn7w3jckvwysh0fm1bavs5";
    };
  });

  # the package set
  haskellPackages865ife =
    callPackage (<nixpkgs> + /pkgs/development/haskell-modules) {
      buildHaskellPackages = super.buildPackages.haskell.packages.ghc865ife;
      ghc = compiler865ife;
      compilerConfig = callPackage (<nixpkgs>
        + /pkgs/development/haskell-modules/configuration-ghc-8.6.x.nix) { };
    };

in {

  # put the compiler & package set to standard location
  haskell = super.haskell // {
    compiler = super.haskell.compiler // { ghc865ife = compiler865ife; };
    packages = super.haskell.packages // { ghc865ife = haskellPackages865ife; };
  };

  # make this the default Haskell package set
  haskellPackages = haskellPackages865ife;
  # well the default can still be further overridden by other overlays

}
