# this defines the Nix env for Hadui to be built
(import ./. { }).shellFor { packages = p: with p; [ hadui ]; }
