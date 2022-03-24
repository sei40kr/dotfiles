{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.gnome.geary;
in
{
  options.modules.desktop.apps.gnome.geary = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.geary.enable = mkForce true;
  };
}
