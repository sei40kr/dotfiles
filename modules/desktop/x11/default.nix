{ lib, ... }:

with lib; {
  imports = [ ./startx.nix ./xbindkeys.nix ./xresources.nix ./xsession.nix ];
}
