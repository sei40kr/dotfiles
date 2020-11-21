{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.xsecurelock;
in {
  options.modules.desktop.xsecurelock = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    length = mkOption {
      type = types.int;
      default = 600;
    };

    period = mkOption {
      type = types.int;
      default = 5;
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ xss-lock ];

    modules.desktop.x11.xsession.init = ''
      ${pkgs.xorg.xset}/bin/xset s ${toString cfg.length} ${toString cfg.period}
      ${pkgs.xss-lock}/bin/xss-lock \
        -s "$XDG_SESSION_ID" \
        -n ${pkgs.xsecurelock}/libexec/xsecurelock/dimmer \
        -l \
        -- ${pkgs.xsecurelock}/bin/xsecurelock &
    '';
    modules.desktop.apps.rofi.customEntries."Lock" =
      "${pkgs.systemd}/bin/loginctl lock-session";
  };
}
