{ config, inputs, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  inherit (inputs) waybar-scripts;
  cfg = config.modules.desktop.apps.waybar;
in {
  options.modules.desktop.apps.waybar = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message =
        "The waybar module requires 'modules.desktop.sway.enable = true'.";
    }];

    user.packages = with pkgs; [ dconf gawk playerctl ];

    systemd.user.services = listToAttrs (map (name:
      nameValuePair "waybar_${name}" {
        description =
          "Highly customizable Wayland bar for Sway and Wlroots based compositors.";
        documentation = [ "https://github.com/Alexays/Waybar/wiki/" ];
        partOf = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        requisite = [ "graphical-session.target" ];
        restartTriggers = [
          (hashString "md5"
            config.environment.etc."xdg/waybar/config_${name}".text)
          config.environment.etc."xdg/waybar/style.css".source
        ];
        environment.PATH = mkForce null;
        serviceConfig = {
          ExecStart =
            "${pkgs.waybar}/bin/waybar -c /etc/xdg/waybar/config_${name}";
          ExecReload = "kill -SIGUSR2 $MAINPID";
        };
        wantedBy = [ "sway-session.target" ];
        reloadIfChanged = true;
      }) [ "top" "bottom" ]);

    environment.etc = {
      "xdg/waybar/config_top".text = toJSON
        (import "${configDir}/waybar/config_top.nix" {
          inherit (inputs) waybar-scripts;
        });
      "xdg/waybar/config_bottom".text = toJSON
        (import "${configDir}/waybar/config_bottom.nix" {
          inherit lib;
          inherit (inputs) waybar-scripts;
        });
      "xdg/waybar/style.css".source = "${configDir}/waybar/style.css";
    };

    fonts.fonts = with pkgs; [ material-design-icons ];
  };
}
