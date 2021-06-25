{ pkgs, ... }:

{
  nwg-dock = pkgs.callPackage ./nwg-dock.nix { };
}
