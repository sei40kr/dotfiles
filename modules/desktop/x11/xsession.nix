{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.desktop.x11.xsession;
  xsessionWrapper = pkgs.writeScript "xsession-wrapper" ''
    ${cfg.sessionCommands}
  '';
in {
  options.modules.desktop.x11.xsession = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    autoRepeatDelay = mkOption {
      type = types.int;
      default = 150;
    };

    autoRepeatInterval = mkOption {
      type = types.int;
      default = 30;
    };

    profile = mkOption {
      type = types.lines;
      default = "";
    };

    init = mkOption {
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;

    my.home.xsession = {
      enable = true;
      profileExtra = cfg.profile;
      initExtra = ''
        ${pkgs.xorg.xset}/bin/xset r rate ${toString cfg.autoRepeatDelay} ${
          toString cfg.autoRepeatInterval
        }

        ${cfg.init}
      '';
    };
  };
}
