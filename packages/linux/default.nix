{ pkgs, ... }:

{
  gnomeExtensions = pkgs.callPackage ./gnome-extensions { };
}
