{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.thunar;
in
{
  options.modules.desktop.apps.thunar = {
    enable = mkEnableOption "Thunar file manager";
  };

  config = mkIf cfg.enable {
    programs.thunar = {
      enable = true;
      plugins = with pkgs; [
        xfce.thunar-archive-plugin
        xfce.thunar-media-tags-plugin
        xfce.thunar-volman
      ];
    };

    services.tumbler.enable = true;
    environment.systemPackages = [ pkgs.ffmpegthumbnailer ];

    xdg.mime.defaultApplications."inode/directory" = "thunar.desktop";
  };
}
