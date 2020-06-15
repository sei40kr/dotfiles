{ config, lib, pkgs, ... }:

with lib; {
  imports = [
    ./dbus.nix
    ./dconf.nix
    ./gnome-keyring.nix
    ./gnome-online-accounts.nix
    ./telepathy.nix
  ];
}
