{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.gnome.seahorse;
in
{
  options.modules.desktop.apps.gnome.seahorse = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.seahorse.enable = mkForce true;
  };
}
