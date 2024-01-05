let
  sources = import ./sources.nix { };
  servant-oidcOverlay = _: pkgs: {
    servant-oidc = pkgs.haskell.packages.ghc947.callCabal2nix "servant-oidc" ../. { };
  };
in import (sources.nixpkgs) { overlays = [ servant-oidcOverlay ]; }
