{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.totem;
  package = pkgs.gnome3.totem;
in {
  options.modules.desktop.media.totem.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = [ package ];
    # TODO Use user D-Bus module
    services.dbus = {
      enable = true;
      packages = [ package ];
    };
  };
}
