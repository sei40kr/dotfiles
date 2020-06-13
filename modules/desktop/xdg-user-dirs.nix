{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.desktop.xdgUserDirs.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.xdgUserDirs.enable {
    my.home.xdg.userDirs.enable = true;

    my.xsession.init = ''
      . "''${XDG_CONFIG_HOME:-''${HOME}/.config}/user-dirs.dirs"
    '';
  };
}
