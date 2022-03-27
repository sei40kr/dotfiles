{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gnome;
in
{
  options.modules.desktop.gnome = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      desktopManager.gnome.enable = true;
    };

    environment.gnome.excludePackages = with pkgs.gnome; [
      baobab
      cheese
      eog
      epiphany
      gedit
      gnome-calculator
      gnome-calendar
      gnome-characters
      gnome-clocks
      gnome-contacts
      gnome-font-viewer
      gnome-logs
      gnome-maps
      gnome-music
      pkgs.gnome-photos
      gnome-screenshot
      gnome-system-monitor
      gnome-weather
      nautilus
      pkgs.gnome-connections
      simple-scan
      totem
      # yelp

      evince
      file-roller
      geary
      gnome-disk-utility
      gnome-terminal
      seahorse
      sushi
    ];

    user.packages = with pkgs.gnomeExtensions; [
      dash-to-dock
      places-status-indicator
      removable-drive-menu
      user-themes
      workspace-indicator
    ];

    modules.desktop.wayland.enable = true;

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "org/gnome/desktop/interface" = {
          enable-animations = false;
          # Disable Activities Overview hot corner
          enable-hot-corners = false;
        };
        "org/gnome/desktop/wm/keybindings" = {
          switch-applications = [ ];
          switch-applications-backward = [ ];
          switch-windows = [ "<Super>Tab" ];
          switch-windows-backward = [ "<Shift><Super>Tab" ];
        };
        "org/gnome/shell/extensions/dash-to-dock" = {
          dock-fixed = true;
        };
        "org/gnome/shell/window-switcher" = {
          current-workspace-only = false;
        };
      };
    };

    modules.desktop.fontconfig.enable = true;
    modules.desktop.gtk.enable = true;
    modules.desktop.qt.enable = true;
  };
}
