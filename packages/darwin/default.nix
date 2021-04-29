{ pkgs, ... }:

{
  alfred = pkgs.callPackage ./alfred.nix { };
  corretto_11 = pkgs.callPackage ./corretto_11.nix { };
  notion = pkgs.callPackage ./notion.nix { };
}
