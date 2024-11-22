let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  packages = [ pkgs.haskell-language-server pkgs.cabal-install ];
  inputsFrom = [ (import ./release.nix).pandoc-converter.env ];
}
