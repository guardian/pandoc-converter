let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in {
  pandoc-converter =
    pkgs.haskellPackages.callPackage ./pandoc-converter.nix { };
}
