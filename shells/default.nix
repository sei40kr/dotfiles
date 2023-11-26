{ callPackage }:

{
  rofi = callPackage ./rofi.nix { };

  waybar = callPackage ./waybar.nix { };

  yonmux = callPackage ./yonmux.nix { };
}
