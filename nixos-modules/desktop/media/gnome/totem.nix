{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.gnome.totem;
in
{
  options.modules.desktop.media.gnome.totem = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ gnome.totem ];
  };
}
