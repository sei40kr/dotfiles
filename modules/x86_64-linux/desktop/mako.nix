{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.mako;
in {
  options.modules.desktop.mako = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name} = {
      programs.mako = {
        enable = true;
        font = "sans-serif 10";
      };
      xdg.configFile."mako/config".onChange =
        "$DRY_RUN_CMD ${pkgs.mako}/bin/makoctl reload";
    };
    # TODO Use user D-Bus module
    services.dbus = {
      enable = true;
      packages = with pkgs; [ mako ];
    };
  };
}
