let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  packages = [ pkgs.haskell-language-server ];
  inputsFrom = [ (import ./release.nix).pandoc-composer.env ];
}
