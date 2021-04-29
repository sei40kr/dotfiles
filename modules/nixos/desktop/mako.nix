{ config, home-manager, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let cfg = config.modules.desktop.mako;
in {
  options.modules.desktop.mako = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message =
        "The mako module requires 'modules.desktop.sway.enable = true'.";
    }];

    home-manager.users.${config.user.name} = {
      programs.mako = {
        enable = true;
        font = "sans-serif 10";
      };
      systemd.user.services.mako = {
        Unit = {
          After = [ "graphical-session-pre.target" ];
          Description = "Lightweight Wayland notification daemon";
          Documentation = "man:mako(1)";
          PartOf = [ "graphical-session.target" ];
          # ConditionEnvironment requires systemd v247 to work correctly
          ConditionEnvironment = "WAYLAND_DISPLAY";
          X-Restart-Triggers = let
            configFile =
              config.home-manager.users.${config.user.name}.xdg.configFile;
          in [ (hashString "md5" configFile."mako/config".text) ];
        };
        Service = {
          Type = "dbus";
          BusName = "org.freedesktop.Notifications";
          ExecStart = "${pkgs.mako}/bin/mako";
          ExecReload = "${pkgs.mako}/bin/makoctl reload";
        };
        Install = { WantedBy = [ "graphical-session.target" ]; };
      };
    };
    # TODO Use user D-Bus module
    services.dbus = {
      enable = true;
      packages = with pkgs; [ mako ];
    };
  };
}
