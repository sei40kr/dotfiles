{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my; {
  config = mkIf (config.modules.theme.active == "zelda") {
    home.configFile."wofi/style.css".source = ./wofi/style.css;
    home-manager.users.${config.user.name} = {
      gtk = {
        iconTheme = {
          package = pkgs.papirus-icon-theme;
          name = "Papirus";
        };
        gtk3 = {
          extraConfig.gtk-application-prefer-dark-theme = true;
          extraCss = readFile ./gtk/gtk-3.0/gtk.css;
        };
      };
      programs = {
        mako = {
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
        waybar.style = readFile ./waybar/style.css;
      };
      wayland.windowManager.sway = {
        config = {
          colors = let
            inactive = {
              background = "#202124";
              border = "#202124";
              childBorder = "#202124";
              indicator = "#202124";
              text = "#cccccc";
            };
          in {
            focused = {
              background = "#0f6776";
              border = "#0f6776";
              childBorder = "#0f6776";
              indicator = "#0f6776";
              text = "#ffffff";
            };
            focusedInactive = inactive;
            unfocused = inactive;
            urgent = inactive;
          };
          floating.border = 2;
          window.border = 2;
        };
        extraConfig = ''
          titlebar_border_thickness 2
        '';
      };
    };
    modules = {
      desktop = {
        waybar.theme = {
          audio.icon = {
            default = [ "󰕿" "󰖀" "󰕾" ];
            headphone = "󰋋";
            headset = "󰋎";
            muted = "󰝟";
          };
          network.icon = {
            disconnected = "󰈂";
            ethernet = "󰈁";
            wifi = [ "󰤯" "󰤟" "󰤢" "󰤥" "󰤨" ];
          };
          workspace.icon = {
            default = "󰣐";
            focused = "󰣐";
            urgent = "󰣐";
          };
        };
      };
      services.random-background.imageDirectory = ./backgrounds;
      term.colorschemes.active = "doom-one";
    };
  };
}
