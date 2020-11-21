{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.apps.rofi;
  systemMenuItems = cfg.systemMenuItems // {
    Reboot = "${pkgs.systemd}/bin/systemctl reboot -i";
    Shutdown = "${pkgs.systemd}/bin/systemctl poweroff -i";
    Suspend = "${pkgs.systemd}/bin/systemctl suspend -i";
  };
  system-menu = pkgs.writeShellScriptBin "system-menu" ''
    menu_items=(
      ${
        concatStringsSep " "
        (mapAttrsToList (title: command: escapeShellArgs [ title command ])
          systemMenuItems)
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
  clipmenu = pkgs.writeShellScriptBin "clipmenu" ''
    CLIPMENU_MAJOR_VERSION=5
    CACHE_DIR="''${XDG_RUNTIME_DIR:-''${TMPDIR:-/tmp}}/clipmenu.''${CLIPMENU_MAJOR_VERSION}.''${USER}"

    selection="$1"

    if [[ -z "$selection" ]]; then
      LC_ALL=C ${pkgs.coreutils}/bin/sort -nrk 1 \
        <"''${CACHE_DIR}/line_cache_clipboard" \
        <"''${CACHE_DIR}/line_cache_primary" |
        ${pkgs.coreutils}/bin/cut -d' ' -f2- |
        ${pkgs.gawk}/bin/awk '!seen[$0]++'
    else
      file="''${CACHE_DIR}/$(${pkgs.coreutils}/bin/cksum <<<"$selection")"

      ${pkgs.xsel}/bin/xsel -i --clipboard <"$file"
    fi
  '';
  modi = [ "combi" ] ++ (optionals config.modules.desktop.tools.clipmenu.enable
    [ "clipboard:${clipmenu}/bin/clipmenu" ]);
  combiModi = [ "drun" "system-menu:${system-menu}/bin/system-menu" ];
in {
  options.modules.desktop.apps.rofi = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    systemMenuItems = mkOption {
      type = with types; attrsOf str;
      default = { };
    };

    theme = mkOption {
      type = types.str;
      default = null;
    };

    package = mkOption {
      type = types.package;
      default = pkgs.unstable.rofi;
    };
  };

  config = mkIf cfg.enable {
    my.packages = [ cfg.package ];
    my.home.xdg.configFile."rofi/config.rasi".text = ''
      ${readFile <config/rofi/config.rasi>}

      configuration {
        modi: "${concatStringsSep "," modi}";
        combi-modi: "${concatStringsSep "," combiModi}";
        theme: "${cfg.theme}";
      }
    '';
  };
}
