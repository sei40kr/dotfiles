# ==========================================================
#
#
#   ██████╗  ██████╗ ██╗  ██╗   ██╗██████╗  █████╗ ██████╗
#   ██╔══██╗██╔═══██╗██║  ╚██╗ ██╔╝██╔══██╗██╔══██╗██╔══██╗
#   ██████╔╝██║   ██║██║   ╚████╔╝ ██████╔╝███████║██████╔╝
#   ██╔═══╝ ██║   ██║██║    ╚██╔╝  ██╔══██╗██╔══██║██╔══██╗
#   ██║     ╚██████╔╝███████╗██║   ██████╔╝██║  ██║██║  ██║
#   ╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝
#
#   To learn more about how to configure Polybar
#   go to https://github.com/polybar/polybar
#
#   The README contains a lot of information
#
#==========================================================
{ fcitx, fcitx-status, gnome-pomodoro, gnome-pomodoro_py, lib
, openweathermap-pop, protonvpn-status, youtube-music }:

{
  "bar/top" = {
    modules-left = "ewmh";
    modules-right =
      "youtube-music openweathermap-pop fcitx-status protonvpn-status alsa date";
  };

  "module/ewmh" = {
    type = "internal/xworkspaces";

    # Only show workspaces defined on the same output as the bar
    #
    # Useful if you want to show monitor specific workspaces
    # on different bars
    #
    # Default: false
    pin-workspaces = false;

    # Create click handler used to focus desktop
    # Default: true
    enable-click = true;

    # Create scroll handlers used to cycle desktops
    # Default: true
    enable-scroll = false;

    # Available tags:
    #   <label-monitor>
    #   <label-state> - gets replaced with <label-(active|urgent|occupied|empty)>
    # Default: <label-state>
    format = "<label-state>";

    # Available tokens:
    #   %name%
    # Default: %name%
    label-monitor = "%name%";

    # Available tokens:
    #   %name%
    #   %icon%
    #   %index%
    # Default: %icon%  %name%
    label-active = "%icon%";

    # (unreleased)
    # Available tokens:
    #   %name%
    #   %icon%
    #   %index%
    # Default: %icon%  %name%
    label-occupied = "%icon%";

    # Available tokens:
    #   %name%
    #   %icon%
    #   %index%
    # Default: %icon%  %name%
    label-urgent = "%icon%";

    # Available tokens:
    #   %name%
    #   %icon%
    #   %index%
    # Default: %icon%  %name%
    label-empty = "%icon%";
  };

  "module/youtube-music" = {
    type = "custom/script";
    exec = "${youtube-music}/bin/youtube-music";
    interval = 5;
    format = "%{+u}<label>%{-u}";
    label = "%output%";
  };

  "module/gnome-pomodoro" = {
    type = "custom/script";
    exec = gnome-pomodoro_py;
    tail = true;
    format = "%{+u}<label>%{-u}";
    label = "%output%";
    click-left = "${gnome-pomodoro}/bin/gnome-pomodoro";
  };

  "module/openweathermap-pop" = {
    type = "custom/script";
    exec = "${openweathermap-pop}/bin/openweathermap-pop";
    interval = 600;
    format = "%{+u}<label>%{-u}";
    label = "%output%";
  };

  "module/fcitx-status" = {
    type = "custom/script";
    exec = "${fcitx-status}/bin/fcitx-status";
    exec-if = "[ -x ${lib.escapeShellArg "${fcitx}/bin/fcitx-remote"} ]";
    interval = 1;
    format = "%{+u}<label>%{-u}";
    label = "%output%";
    click-left = "${fcitx}/bin/fcitx-configtool";
  };

  "module/protonvpn-status" = {
    type = "custom/script";
    exec = "${protonvpn-status}/bin/protonvpn-status";
    interval = 5;
    format = "%{+u}%{T2}%{T-} <label>%{-u}";
    label = "%output%";
  };

  "module/alsa" = {
    type = "internal/alsa";
    format-volume = "%{+u}<ramp-volume> <label-volume>%{-u}";
    format-muted = "%{+u}<label-muted>%{-u}";

    # Available tokens:
    #   %percentage% (default)
    label-volume = "%percentage%%";

    label-muted = "";
    ramp-volume-0 = "";
    ramp-volume-1 = "";
    ramp-volume-2 = "";

    ramp-headphones-0 = "";
  };

  "module/wlan" = {
    type = "internal/network";
    interface = "wlp2s0";
    format-connected = "<ramp-signal>%{O8}<label-connected>";
    ramp-signal-0 = "爛";
    ramp-signal-1 = "嵐";
    ramp-signal-2 = "襤";
    ramp-signal-3 = "蠟";
    label-connected = "%essid%";
    format-disconnected = "<label-disconnected>";
    label-disconnected = "來";
  };

  "module/battery" = {
    type = "internal/battery";
    full-at = 99;
    battery = "BAT0";
    adapter = "ADP1";
    poll-interval = 5;
    format-charging = "%{+u}<animation-charging>%{-u}";
    animation-charging-0 = "";
    animation-charging-1 = "";
    animation-charging-2 = "";
    animation-charging-3 = "";
    animation-charging-4 = "";
    animation-charging-5 = "";
    animation-charging-6 = "";
    animation-charging-7 = "";
    animation-charging-8 = "";
    animation-charging-9 = "";
    animation-charging-10 = "";
    animation-charging-framerate = 150;
    format-discharging = "%{+u}<ramp-capacity>%{-u}";
    ramp-capacity-0 = "";
    ramp-capacity-1 = "";
    ramp-capacity-2 = "";
    ramp-capacity-3 = "";
    ramp-capacity-4 = "";
    ramp-capacity-5 = "";
    ramp-capacity-6 = "";
    ramp-capacity-7 = "";
    ramp-capacity-8 = "";
    ramp-capacity-9 = "";
    ramp-capacity-10 = "";
    format-full = "%{+u}%{-u}";
  };

  "module/date" = {
    type = "internal/date";
    interval = 5;
    date = "%b %d %a";
    time = "%H:%M";
    format = "%{+u}<label>%{-u}";
    label = "%date%, %time%";
  };
}
