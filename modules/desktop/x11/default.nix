{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./startx.nix ./xbindkeys.nix ./xsession.nix ];
}
