{ pkgs }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.ghc
    pkgs.zlib
  ];
}
