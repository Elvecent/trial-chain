{ devMode ? false }:
let
  versions = import ./versions.nix;

  pkgs = import (builtins.fetchTarball versions.nixpkgs) {};

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
in
haskellPackages.override (old: {
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
        trial-chain = self.callCabal2nix "trial-chain" ../. {};
      });
})
