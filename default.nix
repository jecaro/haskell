{ pkgs ? import <nixpkgs> {} }:
let
  hsmaster = import ./hsmaster { inherit pkgs; };
  hshang = import ./hshang { inherit pkgs; };
in pkgs.linkFarmFromDrvs "games" [ hsmaster hshang ]
