{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./apps ./term ./i18n ./tools ./xmonad.nix ];
}
