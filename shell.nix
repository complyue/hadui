# this defines the Nix env for Hadui the package to be built
with (import ./. { });
haskellPackages.shellFor {
  packages = p: with p; [ hadui ];
  nativeBuildInputs = [
    pkgs.cabal-install
  ];
  withHoogle = true;
}
