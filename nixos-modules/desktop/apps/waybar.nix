{ config, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir configDir;
  cfg = config.modules.desktop.apps.waybar;

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

  topJSON = toJSON {
    position = "top";
    height = 48;
    margin = "16 16 0 16";
    name = "top";
    modules-left = [ "sway/workspaces" ];
    modules-right = [
      "custom/pomodoro"
      "custom/fcitx"
      "custom/protonvpn"
      "network"
      "pulseaudio"
      "clock"
      "custom/powermenu"
    ];

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

    "custom/pomodoro" = {
      exec = "${binDir}/waybar/pomodoro";
      return-type = "json";
      format = "{icon}${label "{}"}";
      format-icons = {
        "null" = "";
        paused = "${icon "󰏤"}${label " "}";
        pomodoro = "${icon "󱑂"}${label " "}";
        short-break = "${icon "󰅶"}${label " "}";
        long-break = "${icon "󰅶"}${label " "}";
      };
      tooltip = false;
    };

    "custom/fcitx" = {
      exec = "${binDir}/waybar/fcitx";
      return-type = "json";
      interval = 1;
      tooltip = false;
    };

    "custom/protonvpn" = {
      exec = "${binDir}/waybar/protonvpn";
      return-type = "json";
      interval = 5;
      format = "{icon}${label "{}"}";
      format-icons = {
        disconnected = "";
        connected = "${icon "󰕥"}${label " "}";
      };
      tooltip = false;
      escape = true;
    };

    "network" = {
      format-ethernet = "${icon "󰈀"}";
      format-wifi = "${icon "{icon}"}${label " {essid}"}";
      format-disconnected = "${icon "󰤮"}";
      format-icons = [ "󰤟" "󰤢" "󰤥" "󰤨" ];
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
      on-click = "${binDir}/rofi/powermenu";
      tooltip = false;
    };
  };

  bottomJSON = toJSON {
    position = "bottom";
    height = 48;
    margin = "0 16 16 16";
    name = "bottom";
    modules-left = [ "custom/mpris" ];
    modules-right = [ "cpu" "memory" "network" "disk" ];

    "custom/mpris" = {
      exec = "${binDir}/waybar/mpris";
      return-type = "json";
      format = "{icon}${label "{}"}";
      format-icons = {
        stopped = "";
        playing = "${icon "󰝚"}${label " "}";
        paused = "${icon "󰏤"}${label " "}";
      };
      tooltip = false;
      escape = true;
    };

    "cpu" = {
      interval = 10;
      format = "${icon "󰘚"}${label " {usage}%"}";
      tooltip = false;
    };

    "memory" = {
      interval = 30;
      format = "${icon "󰍛"}${label " {used:0.1f}GiB / {total:0.1f}GiB"}";
      tooltip = false;
    };

    "network" = {
      interval = 30;
      format = concatStringsSep (label " ") [
        "${icon "󰁅"}${label " {bandwidthDownOctets}"}"
        "${icon "󰁝"}${label " {bandwidthUpOctets}"}"
      ];
      tooltip = false;
    };

    "disk" = {
      interval = 30;
      format = "${icon "󰆼"}${label " {used} / {total}"}";
      path = "/";
      tooltip = false;
    };
  };
in {
  options.modules.desktop.apps.waybar = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message =
        "The waybar module requires 'modules.desktop.sway.enable = true'.";
    }];

    user.packages = with pkgs; [ dconf gawk playerctl ];

    systemd.user.services = listToAttrs (map (name:
      nameValuePair "waybar-${name}" {
        description =
          "Highly customizable Wayland bar for Sway and Wlroots based compositors.";
        documentation = [ "https://github.com/Alexays/Waybar/wiki/" ];
        partOf = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        requisite = [ "graphical-session.target" ];
        environment.PATH = mkForce null;
        serviceConfig = {
          ExecStart = "${pkgs.waybar}/bin/waybar -c /etc/xdg/waybar/${name}";
          ExecReload = "kill -SIGUSR2 $MAINPID";
        };
        wantedBy = [ "sway-session.target" ];
        reloadIfChanged = true;
      }) [ "top" "bottom" ]);

    environment.etc = {
      "xdg/waybar/top".text = topJSON;
      "xdg/waybar/bottom".text = bottomJSON;
      "xdg/waybar/style.css".source = "${configDir}/waybar/style.css";
    };

    fonts.fonts = with pkgs; [ material-design-icons ];
  };
}
