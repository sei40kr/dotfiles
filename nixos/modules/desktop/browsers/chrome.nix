{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.chrome;

  package = pkgs.google-chrome;

  gmail = pkgs.makeDesktopItem {
    name = "gmail";
    desktopName = "Gmail";
    terminal = false;
    exec = "${package}/bin/google-chrome-stable --profile-directory=Default --app-id=fmgjjmmmlfnkbppncabfkddbjimcfncm";
    icon = "internet-mail";
    startupWMClass = "crx_fmgjjmmmlfnkbppncabfkddbjimcfncm";
  };
  google-calendar = pkgs.makeDesktopItem {
    name = "google-calendar";
    desktopName = "Google Calendar";
    terminal = false;
    exec = "${package}/bin/google-chrome-stable --profile-directory=Default --app-id=kjbdgfilnfhdoflbpgamdcdgpehopbep";
    icon = "calendar";
    startupWMClass = "crx_kjbdgfilnfhdoflbpgamdcdgpehopbep";
  };
  google-maps = pkgs.makeDesktopItem {
    name = "google-maps";
    desktopName = "Google Maps";
    terminal = false;
    exec = "${package}/bin/google-chrome-stable --profile-directory=Default --app-id=mnhkaebcjjhencmpkapnbdaogjamfbcj";
    icon = "web-google-maps";
    startupWMClass = "crx_mnhkaebcjjhencmpkapnbdaogjamfbcj";
  };
  google-photos = pkgs.makeDesktopItem {
    name = "google-photos";
    desktopName = "Google Photos";
    terminal = false;
    exec = "${package}/bin/google-chrome-stable --profile-directory=Default --app-id=ncmjhecbjeaamljdfahankockkkdmedg";
    icon = "image-viewer";
    startupWMClass = "crx_ncmjhecbjeaamljdfahankockkkdmedg";
  };
  youtube-music = pkgs.makeDesktopItem {
    name = "youtube-music";
    desktopName = "YouTube Music";
    terminal = false;
    exec = "${package}/bin/google-chrome-stable --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod";
    icon = "youtube-music-desktop-app";
    startupWMClass = "crx_cinhimbnkkaeohfgghhklpknlkffjgod";
  };
in
{
  options.modules.desktop.browsers.chrome = with types; {
    enable = mkBoolOpt false;

    mail.enable = mkBoolOpt false;
    calendar.enable = mkBoolOpt false;
    maps.enable = mkBoolOpt false;
    photos.enable = mkBoolOpt false;
    music.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [
      package
      (mkIf cfg.mail.enable gmail)
      (mkIf cfg.calendar.enable google-calendar)
      (mkIf cfg.maps.enable google-maps)
      (mkIf cfg.photos.enable google-photos)
      (mkIf cfg.music.enable youtube-music)
    ];
  };
}
