{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.evince;
  package = pkgs.evince;
in {
  options.modules.desktop.media.evince = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ package ];
    # TODO Use user D-Bus module
    services.dbus = {
      enable = true;
      packages = [ package ];
    };
  };
}
