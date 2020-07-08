{ config, lib, pkgs, ... }:

with lib; {
  imports = [
    ./dbus.nix
    ./dconf.nix
    ./evolution-data-server.nix
    ./glib-networking.nix
    ./gnome-keyring.nix
    ./gnome-online-accounts.nix
    ./gsettings-desktop-schemas.nix
    ./telepathy.nix
    ./tracker.nix
    ./tracker-miners.nix
  ];
}
