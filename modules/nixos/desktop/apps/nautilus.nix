{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let package = pkgs.gnome3.nautilus;
in {
  options.modules.desktop.apps.nautilus = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.apps.nautilus.enable {
    user.packages = [ package ];
    services.dbus = {
      enable = true;
      packages = [ package ];
    };
    modules.desktop.gvfs.enable = true;
  };
}
