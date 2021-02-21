{ config, lib, ... }:

with lib;
let cfg = config.modules.desktop.x11.startx;
in {
  options.modules.desktop.x11.startx = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    autorun = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    modules = {
      desktop.x11.xsession.enable = mkForce true;
      shell.zsh.graphicalSessionInit = mkIf cfg.autorun ''
        if [[ -z "$DISPLAY" && "$XDG_VTNR" == 1 ]]; then
          exec startx
        fi
      '';
    };

    services.xserver.displayManager.startx.enable = true;

    home.file.".xinitrc".text = ''
      ./.xsession
    '';
  };
}
