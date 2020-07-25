{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.apps.rofi;
  systemMenu = pkgs.writeScript "rofi-system-menu" ''
    #!${pkgs.runtimeShell}

    menu_items=(
      ${
        concatStringsSep " " (mapAttrsToList
          (title: command: "${escapeShellArg title} ${escapeShellArg command}")
          cfg.systemMenuItems)
      }
    )

    selection="$1"

    if [[ -z "$selection" ]]; then
      for (( i = 0; i < ''${#menu_items[@]}; i+=2 )); do
        echo "''${menu_items[$i]}"
      done
    else
      for (( i = 0; i < ''${#menu_items[@]}; i+=2 )); do
        if [[ "''${menu_items[$i]}" == "$selection" ]]; then
          break
        fi
      done

      eval "''${menu_items[$((++i))]}"
    fi
  '';
in {
  options.modules.desktop.apps.rofi = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    systemMenuItems = mkOption {
      type = with types; attrsOf str;
      default = {
        "Suspend" = "${pkgs.systemd}/bin/systemctl suspend -i";
        "Reboot" = "${pkgs.systemd}/bin/systemctl reboot -i";
        "Shutdown" = "${pkgs.systemd}/bin/systemctl poweroff -i";
      };
    };

    theme = mkOption {
      type = types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    my.packages = [ pkgs.rofi ];
    my.home.xdg.configFile."rofi/config.rasi".text = ''
      ${readFile <config/rofi/config.rasi>}

      configuration {
        modi: "combi${
          optionalString config.modules.desktop.tools.clipmenu.enable
          ",clipboard:${<config/rofi/scripts/clipboard.bash>}"
        }";
        combi-modi: "drun,system-menu:${systemMenu}";
        theme: "${cfg.theme}";
      }
    '';
  };
}
