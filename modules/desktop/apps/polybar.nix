{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.apps.polybar.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.apps.polybar.enable {
    my.home.services.polybar = {
      enable = true;
      config = {
        "section/base".include-file = "${<config/polybar/config>}";

        # NOTE Polybar systemd service can't import user environment variables,
        #      so define some modules here which calls external programs.
        "module/workspaces-xmonad" = {
          type = "custom/script";
          exec = "${pkgs.coreutils}/bin/tail -F /tmp/.xmonad-workspace-log";
          exec-if = "[ -p /tmp/.xmonad-workspace-log ]";
          tail = true;
        };
      };
      script = ''
        polybar top &
        polybar bottom &
      '';
    };
    my.home.home.file."polybar-scripts".source = <config/polybar/scripts>;
    my.home.xdg.configFile."polybar/config".onChange =
      "systemctl --user restart polybar.service";

    my.packages = with pkgs; [ material-design-icons ];
    my.xsession.init = ''
      rm -f /tmp/.xmonad-workspace-log
      mkfifo /tmp/.xmonad-workspace-log
    '';
  };
}
