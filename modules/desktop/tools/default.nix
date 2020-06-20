{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./fcitx.nix ./random-background.nix ./scrot.nix ./xbindkeys.nix ];
}
