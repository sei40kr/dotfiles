{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./deluge.nix
    ./dunst.nix
    ./evince.nix
    ./geary.nix
    ./gnome-calendar.nix
    ./gnome-contacts.nix
    ./gnome-control-center.nix
    ./gnome-pomodoro.nix
    ./nautilus.nix
    ./polybar.nix
    ./rofi.nix
    ./seahorse.nix
    ./slack.nix
  ];
}
