{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.xsecurelock.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.xsecurelock.enable {
    my.packages = with pkgs; [ xss-lock ];

    modules.desktop.x11.xsession.init = ''
      ${pkgs.xorg.xset}/bin/xset s 600 5
      ${pkgs.xss-lock}/bin/xss-lock -s "''${XDG_SESSION_ID}" \
                                    -n ${
                                      escapeShellArg
                                      "${pkgs.xsecurelock}/libexec/xsecurelock/dimmer"
                                    } \
                                    -l \
                                    -- ${
                                      escapeShellArg
                                      "${pkgs.xsecurelock}/bin/xsecurelock"
                                    } &
    '';
    modules.desktop.apps.rofi.systemMenuItems."Lock" =
      "${pkgs.systemd}/bin/loginctl lock-session";
  };
}
