{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./eog.nix ./totem.nix ];
}
