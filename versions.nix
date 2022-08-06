{
  nixpkgs = {
    name = "nixpkgs-unstable-2022-02-10";
    url = "https://github.com/nixos/nixpkgs/archive/5f9b871b72b24f066b1a1e189efd0669f2888c49.tar.gz";
    sha256 = "1lldnr2ik3wwx1mqcq93bs83dp0g0fxvcrgbbjwg9p9brgdfx3sv";
  };
  
  haskellCompiler = "ghc923";
  
  haskellOverrides = {
    stylish-haskell = {
      pkg = "stylish-haskell";
      ver = "0.14.2.0";
      sha256 = "oZSR5UQ+ieMFRq7MvL0sJqOblMk74awvcepuT+JHYtA=";
    };
    
    effectful-core = {
      pkg = "effectful-core";
      ver = "1.2.0.0";
      sha256 = "B6kL4Ok+Az9wnwv2+0+Yt2C4GH+9oLpzBhC3VidpI/8=";
    };
    
    effectful = {
      pkg = "effectful";
      ver = "1.2.0.0";
      sha256 = "lXaZDipykbG3cE0GD1PAcETrvtZ8rcN1rWYL/4Z40y0=";
    };
  };
}
