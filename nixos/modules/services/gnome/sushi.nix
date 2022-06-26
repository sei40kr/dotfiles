{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.gnome.sushi;
in
{
  options.modules.services.gnome.sushi = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.gnome.sushi.enable = mkForce true;
  };
}
