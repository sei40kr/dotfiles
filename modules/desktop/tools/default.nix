{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./clipmenu.nix ./fcitx.nix ./random-background.nix ./scrot.nix ];
}
