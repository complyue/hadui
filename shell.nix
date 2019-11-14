# this defines the Nix env for Hadui the package to be built
(import ./. { }).haskellPackages.shellFor {
  packages = p: with p; [ hadui ];
  #withHoogle = true;
}
