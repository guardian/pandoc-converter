let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in {
  pandoc-composer = pkgs.haskellPackages.callPackage ./pandoc-composer.nix { };
}
