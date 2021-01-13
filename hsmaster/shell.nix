{ pkgs ? import <nixpkgs> {} }:
let
  hsmaster = import ./. { inherit pkgs; };
in
with pkgs;
mkShell {
  inputsFrom = [ hsmaster.env ];
  buildInputs = [
    ghcid
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.implicit-hie
  ];
}
