{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my; {
  config = mkIf (config.modules.theme.active == "zelda") {
    home.configFile."wofi/style.css".source = ./wofi/style.css;
    home-manager.users.${config.user.name}.programs.mako = {
      backgroundColor = "#202124ff";
      borderColor = "#1c1c1cff";
      borderSize = 4;
      borderRadius = 5;
      icons = false;
      margin = "12,16";
      padding = "8,16";
      textColor = "#ccccccff";
      width = 360;
    };

    modules = {
      desktop = {
        gtk.theme = {
          iconTheme = {
            package = pkgs.papirus-icon-theme;
            name = "Papirus";
          };
          theme = {
            package = pkgs.arc-theme;
            name = "Arc-Dark";
          };
        };
        waybar.theme = {
          audio.icon = {
            default = [ "󰕿" "󰖀" "󰕾" ];
            headphone = "󰋋";
            headset = "󰋎";
            muted = "󰝟";
          };
          bluetooth.icon = {
            disabled = "󰂲";
            enabled = "󰂯";
          };
          mpris.icon = {
            pause = "󰏤";
            play = "󰐊";
            previous = "󰒮";
            next = "󰒭";
          };
          network.icon = {
            disconnected = "󰈂";
            ethernet = "󰈁";
            wifi = [ "󰤯" "󰤟" "󰤢" "󰤥" "󰤨" ];
          };
        };
      };
      services.random-background.imageDirectory = ./backgrounds;
      term.colorschemes.active = "doom-one";

      theme = {
        variant = "dark";

        colors = {
          success = "#73d216";
          warning = "#f27835";
          danger = "#fc4138";

          text = "#d3dae3";
          bg = "#383c4a";
          border = "#2b2e39";

          base = "#404552";

          selection = {
            text = "#ffffff";
            bg = "#5294e2";
            border = "#1e61b0";
          };
        };
      };
    };
  };
}
