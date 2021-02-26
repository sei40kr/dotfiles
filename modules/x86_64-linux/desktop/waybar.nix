{ config, home-manager, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let cfg = config.modules.desktop.waybar;
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
      network.icon = {
        disconnected = mkOpt str null;
        ethernet = mkOpt str null;
        wifi = mkOpt (listOf str) null;
      };
      workspace.icon = {
        default = mkOpt str null;
        focused = mkOpt str null;
        urgent = mkOpt str null;
      };
      symbol.icon = mkOpt (nullOr str) null;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name} = {
      programs.waybar = {
        enable = true;
        settings = [
          (let symbolIcon = cfg.theme.symbol.icon;
          in {
            height = 36;
            layer = "top";
            modules = {
              clock = {
                format = "{:%b %d %a %H:%M}";
                timezone = "Asia/Tokyo";
                tooltip = false;
              };
              "custom/symbol" =
                mkIf (symbolIcon != null) { format = "${symbolIcon}"; };
              network = let networkIcon = cfg.theme.network.icon;
              in {
                format-disconnected = networkIcon.disconnected;
                format-ethernet = networkIcon.ethernet;
                format-icons = networkIcon.wifi;
                format-wifi = "{icon}  {essid}";
                tooltip = false;
              };
              pulseaudio = let audioIcon = cfg.theme.audio.icon;
              in {
                format = "{icon}  {volume}%";
                format-icons = {
                  default = audioIcon.default;
                  headphone = audioIcon.headphone;
                  headset = audioIcon.headset;
                };
                format-muted = audioIcon.muted;
                tooltip = false;
              };
              "sway/workspaces" = {
                all-outputs = true;
                disable-scroll = true;
                format = "{icon}";
                format-icons = let workspaceIcon = cfg.theme.workspace.icon;
                in {
                  default = workspaceIcon.default;
                  focused = workspaceIcon.focused;
                  persistent = workspaceIcon.default;
                  urgent = workspaceIcon.urgent;
                };
                persistent_workspaces = {
                  "1" = [ ];
                  "2" = [ ];
                  "3" = [ ];
                  "4" = [ ];
                };
              };
            };

            modules-left = [ "sway/workspaces" ];
            modules-center = mkIf (symbolIcon != null) [ "custom/symbol" ];
            modules-right = [ "network" "pulseaudio" "clock" ];

            position = "top";
          })
        ];
        systemd.enable = true;
      };
      systemd.user.services.waybar.Unit = {
        After = [ "graphical-session-pre.target" ];
        X-Restart-Triggers = let
          configFile =
            config.home-manager.users.${config.user.name}.xdg.configFile;
        in [
          "${configFile."waybar/config".source}"
          (hashString "md5" configFile."waybar/style.css".text)
        ];
      };
    };
    # TODO Use user-level fonts
    fonts.fonts = with pkgs; [ material-design-icons ];
  };
}
