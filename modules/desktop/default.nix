{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./term ./xmonad.nix ];
}
