{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.gnome.file-roller;
in
{
  options.modules.desktop.apps.gnome.file-roller = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ gnome.file-roller ];
  };
}
