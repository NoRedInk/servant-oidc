let
  sources = import ./sources.nix { };
  servant-oidcOverlay = _: pkgs: {
    servant-oidc = pkgs.haskellPackages.callCabal2nix "servant-oidc" ../. { };
  };
in import (sources.nixpkgs) { overlays = [ servant-oidcOverlay ]; }
