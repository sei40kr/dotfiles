{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  clipmenuEnabled = config.modules.desktop.tools.clipmenu.enable;

  cfg = config.modules.desktop.apps.rofi;
  customEntries = cfg.customEntries // {
    Reboot = "${pkgs.systemd}/bin/systemctl reboot -i";
    Shutdown = "${pkgs.systemd}/bin/systemctl poweroff -i";
    Suspend = "${pkgs.systemd}/bin/systemctl suspend -i";
  };

  clipmenu = pkgs.my.rofiScripts.clipmenu;
  custom-entries = pkgs.writeShellScriptBin "custom-entries" ''
    entries=(
      ${
        concatStringsSep " "
        (mapAttrsToList (text: command: escapeShellArgs [ text command ])
          customEntries)
      }
    )

    if [[ "$ROFI_RETV" == 1 ]]; then
      eval "$ROFI_INFO"
      exit
    fi

    for (( i = 0; i < ''${#entries[@]}; i++ )); do
      text="''${entries[$i]}"
      command="''${entries[$((++i))]}"
      echo -en "''${text}\0info\x1f''${command}\n"
    done
  '';

  modi = [ "combi" ]
    ++ (optionals clipmenuEnabled [ "clipboard:${clipmenu}/bin/clipmenu" ]);
  combiModi = [ "drun" "custom-entries:${custom-entries}/bin/custom-entries" ];
in {
  options.modules.desktop.apps.rofi = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    customEntries = mkOption {
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
    user.packages = [ cfg.package ];
    home.configFile."rofi/config.rasi".text = ''
      ${readFile "${configDir}/rofi/config.rasi"}

      configuration {
        modi: "${concatStringsSep "," modi}";
        combi-modi: "${concatStringsSep "," combiModi}";
        theme: "${cfg.theme}";
      }
    '';
  };
}
