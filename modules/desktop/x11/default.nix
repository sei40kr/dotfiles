{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./startx.nix ./xbindkeys.nix ./xresources.nix ./xsession.nix ];
}
