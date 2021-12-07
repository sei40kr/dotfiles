{ lib, waybar-scripts }:

let
  icon = icon: "<span font_desc='Material Design Icons 14'>${icon}</span>";
  # (14pt - 12pt) / 2 / 12 * 10000 = 833
  label = label: "<span rise='833'>${label}</span>";

  singleSpace = "<span font_desc='monospace 100%'> </span>";
in {
  position = "bottom";
  height = 48;
  margin = "0 16 16 16";
  name = "bottom";
  modules-left = [ "custom/mpris-track-info" ];
  modules-right = [ "cpu" "memory" "network" "disk" ];

  "custom/mpris-track-info" = {
    exec = "${waybar-scripts}/mpris-track-info-tail/mpris-track-info-tail.bash";
    return-type = "json";
    format = "{icon}${label "{}"}";
    format-icons = {
      stopped = "";
      playing = "${icon "󰝚"}${singleSpace}";
      paused = "${icon "󰏤"}${singleSpace}";
    };
    tooltip = false;
    escape = true;
  };

  "cpu" = {
    interval = 10;
    format = "${icon "󰘚"}${singleSpace}${label "{usage}%"}";
    tooltip = false;
  };

  "memory" = {
    interval = 30;
    format =
      "${icon "󰍛"}${singleSpace}${label " {used:0.1f}GiB / {total:0.1f}GiB"}";
    tooltip = false;
  };

  "network" = {
    interval = 30;
    format = lib.concatStringsSep singleSpace [
      "${icon "󰁅"}${singleSpace}${label "{bandwidthDownOctets}"}"
      "${icon "󰁝"}${singleSpace}${label "{bandwidthUpOctets}"}"
    ];
    tooltip = false;
  };

  "disk" = {
    interval = 30;
    format = "${icon "󰆼"}${singleSpace}${label "{used} / {total}"}";
    path = "/";
    tooltip = false;
  };
}
