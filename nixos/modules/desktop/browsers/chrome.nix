{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.chrome;

  gmail = pkgs.makeDesktopItem {
    name = "web-gmail";
    desktopName = "Gmail";
    terminal = false;
    exec = "${pkgs.google-chrome}/bin/google-chrome-stable --profile-directory=Default --app-id=fmgjjmmmlfnkbppncabfkddbjimcfncm";
    icon = "web-google-gmail";
    startupWMClass = "crx_fmgjjmmmlfnkbppncabfkddbjimcfncm";
  };
  google-calendar = pkgs.makeDesktopItem {
    name = "web-google-calendar";
    desktopName = "Google Calendar";
    terminal = false;
    exec = "${pkgs.google-chrome}/bin/google-chrome-stable --profile-directory=Default --app-id=kjbdgfilnfhdoflbpgamdcdgpehopbep";
    icon = "calendar";
    startupWMClass = "crx_kjbdgfilnfhdoflbpgamdcdgpehopbep";
  };
  google-maps = pkgs.makeDesktopItem {
    name = "web-google-maps";
    desktopName = "Google Maps";
    terminal = false;
    exec = "${pkgs.google-chrome}/bin/google-chrome-stable --profile-directory=Default --app-id=mnhkaebcjjhencmpkapnbdaogjamfbcj";
    icon = "web-google-maps";
    startupWMClass = "crx_mnhkaebcjjhencmpkapnbdaogjamfbcj";
  };
  google-photos = pkgs.makeDesktopItem {
    name = "web-google-photos";
    desktopName = "Google Photos";
    terminal = false;
    exec = "${pkgs.google-chrome}/bin/google-chrome-stable --profile-directory=Default --app-id=ncmjhecbjeaamljdfahankockkkdmedg";
    icon = "photos";
    startupWMClass = "crx_ncmjhecbjeaamljdfahankockkkdmedg";
  };
  youtube-music = pkgs.makeDesktopItem {
    name = "web-youtube-music";
    desktopName = "YouTube Music";
    terminal = false;
    exec = "${pkgs.google-chrome}/bin/google-chrome-stable --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod";
    icon = "youtube-music-desktop-app";
    startupWMClass = "crx_cinhimbnkkaeohfgghhklpknlkffjgod";
  };
in
{
  options.modules.desktop.browsers.chrome = with types; {
    enable = mkBoolOpt false;

    webapps = {
      gmail.enable = mkBoolOpt false;
      google-calendar.enable = mkBoolOpt false;
      google-maps.enable = mkBoolOpt false;
      google-photos.enable = mkBoolOpt false;
      youtube-music.enable = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    user.packages = [
      pkgs.google-chrome
      (mkIf cfg.webapps.gmail.enable gmail)
      (mkIf cfg.webapps.google-calendar.enable google-calendar)
      (mkIf cfg.webapps.google-maps.enable google-maps)
      (mkIf cfg.webapps.google-photos.enable google-photos)
      (mkIf cfg.webapps.youtube-music.enable youtube-music)
    ];

    environment.etc."opt/chrome/policies/managed/extra.json".text = builtins.toJSON {
      WebAppInstallForceList = optionals cfg.webapps.gmail.enable [
        {
          custom_name = "Gmail";
          default_launch_container = "window";
          install_as_shortcut = true;
          url = "https://mail.google.com/";
        }
      ] ++ optionals cfg.webapps.google-calendar.enable [
        {
          custom_name = "Google Calendar";
          default_launch_container = "window";
          install_as_shortcut = true;
          url = "https://calendar.google.com/";
        }
      ] ++ optionals cfg.webapps.google-maps.enable [
        {
          custom_name = "Google Maps";
          default_launch_container = "window";
          install_as_shortcut = false;
          url = "https://www.google.com/maps";
        }
      ] ++ optionals cfg.webapps.google-photos.enable [
        {
          custom_name = "Google Photos";
          default_launch_container = "window";
          install_as_shortcut = false;
          url = "https://photos.google.com/";
        }
      ] ++ optionals cfg.webapps.youtube-music.enable [
        {
          custom_name = "YouTube Music";
          default_launch_container = "window";
          install_as_shortcut = false;
          url = "https://music.youtube.com/";
        }
      ];
    };
  };
}
