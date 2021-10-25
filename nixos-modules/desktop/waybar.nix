{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.waybar;

  fonts = {
    base.size = 11;
    icon = {
      size = 14;
      family = "Material Design Icons";
    };
  };
  markup = { family ? null, size ? null, rise ? null }:
    text:
    "<span${
      optionalString (family != null || size != null)
      " font_desc='${toString family} ${toString size}'"
    }${
      optionalString (rise != null)
      # HACK floating-point number -> integer
      " rise='${head (splitString "." (toString rise))}'"
    }>${text}</span>";
  icon = markup (let base = fonts.base.size;
  in rec {
    inherit (fonts.icon) family size;
    rise = -5000 - (size - base) * 1.0 / size / 2 * 10000;
  });
  label = markup { rise = -5000; };

  configJSON = builtins.toJSON {
    position = "top";
    height = 48;
    margin = "16 16 0 16";
    modules-left = [ "sway/workspaces" ];
    modules-right = [ "custom/fcitx" "pulseaudio" "clock" "custom/powermenu" ];

    "sway/workspaces" = {
      format = icon "{icon}";
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

    "custom/fcitx" = {
      exec = "$DOTFILES_BIN/waybar/fcitx";
      return-type = "json";
      interval = 1;
      tooltip = false;
    };

    pulseaudio = {
      format = "${icon "{icon}"}${label " {volume}%"}";
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
      format = icon "{icon}";
      format-icons = "󰐥";
      on-click = "$DOTFILES_BIN/rofi/powermenu";
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
