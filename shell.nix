# this defines the Nix env for Hadui the package to be built
with (import ./. { });
haskellPackages.hadui.envFunc { withHoogle = true; }
