{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./input ./term ./xmonad.nix ];
}
