{ ... }:

{
  imports = [
    ./dconf.nix
    ./fontconfig.nix
    ./gdm.nix
    ./gtk.nix
    ./swaylock.nix
    ./apps
    ./browsers
    ./de
    ./media
    ./qt
    ./themes
    ./wm
  ];
}
