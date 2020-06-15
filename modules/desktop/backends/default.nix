{ config, lib, pkgs, ... }:

with lib; {
  imports = [
    ./dbus.nix
    ./dconf.nix
    ./evolution-data-server.nix
    ./glib-networking.nix
    ./gnome-keyring.nix
    ./gnome-online-accounts.nix
    ./telepathy.nix
  ];
}
