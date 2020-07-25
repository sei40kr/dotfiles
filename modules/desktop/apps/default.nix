{ lib, ... }:

with lib; {
  imports = [
    ./bitwarden.nix
    ./cheese.nix
    ./dunst.nix
    ./evince.nix
    ./geary.nix
    ./gnome-books.nix
    ./gnome-calendar.nix
    ./gnome-contacts.nix
    ./gnome-control-center.nix
    ./gnome-file-roller.nix
    ./gnome-font-viewer.nix
    ./gnome-pomodoro.nix
    ./nautilus.nix
    ./polybar.nix
    ./rofi.nix
    ./seahorse.nix
    ./slack.nix
  ];
}
