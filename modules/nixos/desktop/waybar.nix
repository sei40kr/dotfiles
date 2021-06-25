{ config, home-manager, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  cfg = config.modules.desktop.waybar;
  wrapper = pkgs.writeShellScriptBin "waybar" ''
    pid=$(${pkgs.procps}/bin/pgrep -x sway)
    SWAYSOCK=''${XDG_RUNTIME_DIR:-/run/user/$UID}/sway-ipc.$UID.$pid.sock
    if [[ -S "$SWAYSOCK" ]]; then
      export SWAYSOCK
    fi
    ${pkgs.waybar}/bin/waybar "$@"
  '';
  style = readFile "${configDir}/waybar/style.css";
in {
  options.modules.desktop.waybar = with types; {
    enable = mkBoolOpt false;
    theme = {
      audio.icon = {
        default = mkOpt (listOf str) null;
        headphone = mkOpt str null;
        headset = mkOpt str null;
        muted = mkOpt str null;
      };
      bluetooth.icon = {
        enabled = mkOpt str null;
        disabled = mkOpt str null;
      };
      network.icon = {
        ethernet = mkOpt str null;
        wifi = mkOpt (listOf str) null;
        disconnected = mkOpt str null;
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message =
        "The swaybg module requires 'modules.desktop.sway.enable = true'.";
    }];

    home-manager.users.${config.user.name} = {
      programs.waybar = {
        inherit style;
        enable = true;
        package = wrapper;
        settings = [{
          height = 36;
          layer = "bottom";
          modules = {
            bluetooth = {
              format-icons = {
                inherit (cfg.theme.bluetooth.icon) enabled disabled;
              };
              tooltip = false;
            };
            clock = {
              format = "{:%b %e %R}";
              timezone = "Asia/Tokyo";
              tooltip = false;
            };
            network = {
              format-ethernet = cfg.theme.network.icon.ethernet;
              format-wifi = "{icon}";
              format-disconnected = cfg.theme.network.icon.disconnected;
              format-icons = cfg.theme.network.icon.wifi;
              tooltip = false;
            };
            pulseaudio = {
              format = "{icon}";
              format-icons = {
                default = cfg.theme.audio.icon.default;
                headphone = cfg.theme.audio.icon.headphone;
                headset = cfg.theme.audio.icon.headset;
              };
              format-muted = cfg.theme.audio.icon.muted;
              tooltip = false;
            };
            "sway/workspaces" = {
              disable-scroll = true;
              disable-click = true;
              current-only = true;
            };
          };

          modules-left = [ "sway/workspaces" ];
          modules-center = [ "clock" ];
          modules-right = [ "bluetooth" "pulseaudio" "network" ];

          position = "top";
        }];
      };
      systemd.user.services.waybar = {
        Unit = {
          After = [ "sway-session.target" ];
          Description =
            "Highly customizable Wayland bar for Sway and Wlroots based compositors.";
          Documentation = "https://github.com/Alexays/Waybar/wiki";
          PartOf = [ "sway-session.target" ];
          X-Restart-Triggers = [
            "${config.home-manager.users.${config.user.name}.xdg.configFile."waybar/config".source}"
            (hashString "md5" style)
          ];
        };
        Service = {
          Type = "simple";
          ExecStart = "${wrapper}/bin/waybar";
          Restart = "always";
          RestartSec = "1sec";
        };
        Install.WantedBy = [ "sway-session.target" ];
      };
    };
    # TODO Use user-level fonts
    fonts.fonts = with pkgs; [ material-design-icons ];
  };
}
