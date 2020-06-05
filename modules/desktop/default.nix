{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./xmonad.nix ];
}
