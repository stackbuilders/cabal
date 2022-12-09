{ pkgs }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.ghc94
    pkgs.zlib
  ];
}
