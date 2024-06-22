{ callPackage }:

{
  rofi = callPackage ./rofi.nix { };

  waybar = callPackage ./waybar.nix { };

  xmonad = callPackage ./xmonad.nix { };

  yonmux = callPackage ./yonmux.nix { };
}
