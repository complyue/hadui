{ pkgs, stdenv, haskellPackages, mkDerivation }:
with haskellPackages;
mkDerivation {
  pname = "hadui";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  homepage = "https://github.com/complyue/hadui";
  description = "Web front UI for interactive Haskell projects";
  license = stdenv.lib.licenses.bsd3;

  # to workaround missing zlib.h problem during build
  librarySystemDepends = [ pkgs.zlib ];
}
