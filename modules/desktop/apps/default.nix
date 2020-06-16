{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./web-browsers
    ./deluge.nix
    ./dunst.nix
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
