{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./apps ./i18n ./term ./xmonad.nix ];
}
