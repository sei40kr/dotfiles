{ config, home-manager, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.xdgUserDirs.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.desktop.xdgUserDirs.enable {
    modules.desktop.x11.xsession.profile = ''
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

    user.packages = with pkgs; [ xdg-user-dirs ];
    home-manager.users.${config.user.name}.xdg.userDirs.enable = true;
  };
}
