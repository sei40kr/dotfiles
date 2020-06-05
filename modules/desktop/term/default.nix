{ config, lib, pkgs, ... }:

with lib; {
  imports = [ ./alacritty.nix ];
}
