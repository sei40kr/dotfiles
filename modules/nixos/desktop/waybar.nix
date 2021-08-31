{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.waybar;

  configJSON = builtins.toJSON {
    position = "top";
    height = 44;
    modules-left = [ "custom/appmenu" "sway/workspaces" ];
    modules-center = [ "clock" ];
    modules-right = [ "custom/imestatus" "pulseaudio" "custom/powermenu" ];

    "custom/appmenu" = {
      format = "{icon}";
      format-icons = "󰀻";
      on-click = "rofi-appmenu_topleft";
      tooltip = false;
    };
    "sway/workspaces" = {
      format = "{icon}";
      format-icons = {
        "1" = "󰖟";
        "2" = "󰅩";
        "3" = "󰉋";
        default = "󰐽";
        urgent = "󰐽";
        focused = "󰐾";
      };
      disable-scroll = true;
      persistent_workspaces = [ "1" "2" "3" ];
      tooltip = false;
    };

    "custom/imestatus" = {
      exec = "${configDir}/waybar/scripts/imestatus.bash";
      interval = 1;
      tooltip = false;
    };
    pulseaudio = {
      format = "{icon}";
      format-icons = {
        default = [ "󰖀" "󰕾" ];
        headphone = "󰋋";
      };
      format-muted = "󰸈";
      tooltip = false;
    };
    clock = {
      format = "{:%b %e, %H:%M}";
      tooltip = false;
    };
    "custom/powermenu" = {
      format = "{icon}";
      format-icons = "󰐥";
      on-click = "rofi-powermenu_topright";
      tooltip = false;
    };
  };
in {
  options.modules.desktop.waybar = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message =
        "The waybar module requires 'modules.desktop.sway.enable = true'.";
    }];

    user.packages = with pkgs; [ waybar ];

    # TODO Specify stylesheet
    environment.etc = {
      "xdg/waybar/config".text = configJSON;
      "sway/config.d/startup/waybar.conf".text = ''
        exec ${pkgs.waybar}/bin/waybar
      '';
    };
    fonts.fonts = with pkgs; [ material-design-icons ];
  };
}
