{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.sushi;
in {
  options.modules.services.sushi.enable = mkBoolOpt false;

  config.services.gnome.sushi.enable = cfg.enable;
}
