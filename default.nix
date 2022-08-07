{ devMode ? false }:
let
  versions = import ./versions.nix;

  pkgs = import (builtins.fetchTarball versions.nixpkgs) {};

  whenDev = stuff: if devMode then stuff else [];

  inherit (pkgs.lib.attrsets) mapAttrs;
  
  haskellPackages = pkgs.haskell.packages.${versions.haskellCompiler}.override {
    overrides = self: super:
      mapAttrs (_: v:
        if    builtins.typeOf v == "set"
           && builtins.hasAttr "isHaskellLibrary" v
           && v.isHaskellLibrary
        then pkgs.haskell.lib.dontCheck v
        else v) super;
  };
  
  myHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides (self: super: with pkgs.haskell.lib;
      let
        new =
          mapAttrs (_: value:
            dontCheck (self.callHackageDirect value {})
          ) versions.haskellOverrides;
      in
        new // {
          stylish-haskell = overrideCabal
            new.stylish-haskell ( old: {
              configureFlags = [ "-f +ghc-lib" ];
            });
          trial-chain = self.callCabal2nix "trial-chain" ./. {};
        });
  });
    
  projectShell = myHaskellPackages.shellFor {
    packages = p:
      [ p.trial-chain
      ];
    buildInputs = whenDev [
      myHaskellPackages.stylish-haskell
    ];
    withHoogle = devMode;
  };
in
if pkgs.lib.inNixShell then projectShell else myHaskellPackages
#pkgs.haskell.lib.dontCheck myHaskellPackages.trial-chain
