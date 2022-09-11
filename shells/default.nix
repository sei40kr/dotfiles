{ pkgs }:

{
  waybar = pkgs.callPackage ./waybar.nix { };

  yonmux = pkgs.callPackage ./yonmux.nix { };
}
