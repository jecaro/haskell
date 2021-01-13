{ pkgs ? import <nixpkgs> {} }:
let
  hshang = import ./. { inherit pkgs; };
in
with pkgs;
mkShell {
  inputsFrom = [ hshang.env ];
  buildInputs = [
    ghcid
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.implicit-hie
  ];
}
