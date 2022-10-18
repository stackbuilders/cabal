{ pkgs }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.ghc924
    pkgs.zlib
  ];
}
