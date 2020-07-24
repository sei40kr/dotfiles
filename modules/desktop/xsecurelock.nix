{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.xsecurelock.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.xsecurelock.enable {
    my.home.services.screen-locker = {
      enable = true;
      lockCmd = "${pkgs.xsecurelock}/bin/xsecurelock";
      xssLockExtraOptions =
        [ "-n" "${pkgs.xsecurelock}/libexec/xsecurelock/dimmer" "-l" ];
    };

    my.packages = with pkgs; [ xss-lock ];
  };
}
