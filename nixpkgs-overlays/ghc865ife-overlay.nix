self: super:
let

  # useful artifacts
  inherit (super) haskell newScope fetchurl;
  haskellLib = haskell.lib;

  # mock the env at (<nixpkgs> + /pkgs/top-level/haskell-packages)
  callPackage = newScope {
    inherit haskellLib;
    overrides = haskell.packageOverrides;
  };

  # the compiler
  compiler865ife = haskell.compiler.ghc865.overrideAttrs (oldAttrs: {
    name = "${oldAttrs.name}-ife";

    src = fetchurl {
      name = "ghc-8.6.5-ife-src.tar.xz";
      url =
        "https://gitlab.haskell.org/complyue/ghc-ife-sdist/raw/master/ghc-8.6.5-src.tar.xz";
      sha256 = "0wf5v1ry3rlwhbsxlvka3qscdb4jz4jn7w3jckvwysh0fm1bavs5";
    };
  });

  # the package set
  haskellPackages865ife =
    callPackage (<nixpkgs> + /pkgs/development/haskell-modules) {
      buildHaskellPackages = haskell.packages.ghc865;
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

  # make this the default Haskell package set,
  # well the default can still be further overridden by other overlays
  haskellPackages = haskellPackages865ife;

}
