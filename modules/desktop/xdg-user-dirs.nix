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
      export XDG_DESKTOP_DIR
      export XDG_DOCUMENTS_DIR
      export XDG_DOWNLOAD_DIR
      export XDG_MUSIC_DIR
      export XDG_PICTURES_DIR
      export XDG_PUBLICSHARE_DIR
      export XDG_TEMPLATES_DIR
      export XDG_VIDEOS_DIR
    '';
  };
}
