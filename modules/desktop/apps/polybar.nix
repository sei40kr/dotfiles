{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.apps.polybar;
  polybarConfig = pkgs.writeText "polybar-config" ''
    [section/base]
    include-file = ${cfg.themeConfig}

    ${readFile <config/polybar/config>}
  '';
  pythonEnv = pkgs.python3.withPackages
    (pythonPackages: with pythonPackages; [ pygobject3 dbus-python ]);
  polybarStart = pkgs.writeShellScriptBin "polybar-start" "PATH=${
      escapeShellArg "${pythonEnv}/bin"
    }:\${PATH} ${pkgs.polybar}/bin/polybar top &";
in {
  options.modules.desktop.apps.polybar = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    themeConfig = mkOption { type = with types; either path str; };
  };

  config = mkIf cfg.enable {
    my.home.services.polybar = {
      enable = true;
      config = {
        "section/base".include-file = "${polybarConfig}";
        "module/gnome-pomodoro" = {
          exec = "${<config/polybar/scripts/gnome-pomodoro.py>}";
          click-left = "${pkgs.gnome3.pomodoro}/bin/gnome-pomodoro";
        };
        "module/fcitx" = {
          exec = "${<config/polybar/scripts/fcitx.py>}";
          exec-if = "[ -x ${escapeShellArg "${pkgs.fcitx}/bin/fcitx-remote"} ]";
          click-left = "${pkgs.fcitx}/bin/fcitx-configtool";
        };
      };
      script = "";
    };

    my.packages = with pkgs; [ material-design-icons ];
    my.home.systemd.user.services.polybar = {
      Unit = {
        Requires = [ "dbus.service" ];
        X-Restart-Triggers = [ "${<config/polybar/config>}" ];
      };
      Service = {
        Environment = mkForce "";
        ExecStart = mkForce "${polybarStart}/bin/polybar-start";
      };
    };
  };
}
