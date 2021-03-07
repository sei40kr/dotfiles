{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.dconf;
  package = pkgs.dconf;
in {
  options.modules.desktop.dconf = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    modules.desktop.env.GIO_EXTRA_MODULES =
      [ "${package.lib}/lib/gio/modules" ];
    home-manager.users.${config.user.name}.dconf.enable = true;
    # TODO Use user D-Bus module
    services.dbus = {
      enable = true;
      packages = [ package ];
    };
  };
}
