{ pkgs }:

{
  rofi = pkgs.callPackage ./rofi.nix { };

  waybar = pkgs.callPackage ./waybar.nix { };

  yonmux = pkgs.callPackage ./yonmux.nix { };
}
