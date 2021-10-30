{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.mako;
in {
  options.modules.desktop.mako = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message =
        "The mako module requires 'modules.desktop.sway.enable = true'.";
    }];

    user.packages = with pkgs; [ mako ];

    systemd.user.services.mako = {
      description = "Lightweight Wayland notification daemon";
      documentation = [ "man:mako(1)" ];
      partOf = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecCondition =
          "${pkgs.runtimeShell} -c '[[ -n \"$WAYLAND_DISPLAY\" ]]'";
        ExecStart = "${pkgs.mako}/bin/mako";
        ExecReload = "${pkgs.mako}/bin/makoctl reload";
      };
      wantedBy = [ "graphical-session.target" ];
      reloadIfChanged = true;
    };

    home.configFile."mako/config".source = "${configDir}/mako/config";
  };
}
