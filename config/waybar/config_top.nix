{ waybar-scripts }:

let
  icon = icon: "<span font_desc='Material Design Icons 14'>${icon}</span>";
  # (14pt - 12pt) / 2 / 12 * 10000 = 833
  label = label: "<span rise='833'>${label}</span>";

  singleSpace = "<span font_desc='monospace 100%'> </span>";
in {
  position = "top";
  height = 48;
  name = "top";
  modules-left = [ "sway/workspaces" ];
  modules-center = [ "clock" ];
  modules-right = [
    "custom/gnome-pomodoro-timer"
    "custom/fcitx5-status"
    "custom/protonvpn-status"
    "network"
    "pulseaudio"
    "custom/powermenu"
  ];

  "sway/workspaces" = {
    format = icon "{icon}";
    format-icons = {
      "1" = "󰖟";
      "2" = "󰅩";
      "3" = "󰉏";
      "4" = "󰭹";
      default = "󰐽";
      urgent = "󰐽";
      focused = "󰐾";
    };
    disable-scroll = true;
    persistent_workspaces = [ "1" "2" "3" "4" ];
    tooltip = false;
  };

  clock = {
    format = "{:%b %e, %H:%M}";
    tooltip = false;
  };

  "custom/gnome-pomodoro-timer" = {
    exec =
      "${waybar-scripts}/gnome-pomodoro-timer-tail/gnome-pomodoro-timer-tail.bash";
    return-type = "json";
    format = "{icon}${label "{}"}";
    format-icons = {
      "null" = "";
      paused = "${icon "󰏤"}${singleSpace}";
      pomodoro = "${icon "󱑂"}${singleSpace}";
      short-break = "${icon "󰅶"}${singleSpace}";
      long-break = "${icon "󰅶"}${singleSpace}";
    };
    tooltip = false;
  };

  "custom/fcitx5-status" = {
    exec = "${waybar-scripts}/fcitx5-status/fcitx5-status.bash";
    return-type = "json";
    interval = 1;
    tooltip = false;
  };

  "custom/protonvpn-status" = {
    exec = "${waybar-scripts}/protonvpn-status/protonvpn-status.bash";
    return-type = "json";
    interval = 5;
    format = icon "{icon}";
    format-icons = {
      disconnected = "";
      connected = "󰕥";
    };
    escape = true;
  };

  "network" = {
    format-ethernet = icon "󰈀";
    format-wifi = "${icon "{icon}"}${singleSpace}${label "{essid}"}";
    format-disconnected = icon "󰤮";
    format-icons = [ "󰤟" "󰤢" "󰤥" "󰤨" ];
    tooltip = false;
  };

  pulseaudio = {
    format = icon "{icon}";
    format-icons = {
      default = [ "󰖀" "󰕾" ];
      headphone = "󰋋";
    };
    format-muted = icon "󰸈";
  };

  "custom/powermenu" = {
    format = icon "{icon}";
    format-icons = "󰐥";
    tooltip = false;
  };
}
