{}:
let
  haskellPackages = import ./nix/haskellPackages.nix {};
in
haskellPackages.trial-chain
