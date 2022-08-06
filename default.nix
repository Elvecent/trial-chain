{ devMode ? false }:
let
  versions = import ./versions.nix;

  pkgs = import (builtins.fetchTarball versions.nixpkgs) {};

  whenDev = stuff: if devMode then stuff else [];

  inherit (pkgs.lib.attrsets) mapAttrs;
  
  haskellPackages =
    pkgs.haskell.packages.${versions.haskellCompiler}.override {
      overrides = self: super: with pkgs.haskell.lib;
        mapAttrs (_: value: dontCheck value) super;
    };
  
  myHaskellPackages = haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib;
      let new =
            mapAttrs (_: value:
              dontCheck (self.callHackageDirect value {})
            ) versions.haskellOverrides;
      in new // {
        stylish-haskell = overrideCabal
          new.stylish-haskell ( old: {
            configureFlags = [ "-f +ghc-lib" ];
          });
        trial-chain = overrideCabal
          (self.callCabal2nix "trial-chain" ./. {}) (old: {
            libraryHaskellDepends = old.libraryHaskellDepends ++ (whenDev [
              self.pretty-simple
            ]);
          });
      };
  };
    
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
