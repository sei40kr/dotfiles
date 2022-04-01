{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  shellThemeType = with types; submodule {
    options = {
      package = mkOpt package null;
      name = mkOpt str null;
    };
  };

  cfg = config.modules.desktop.gnome;

  exts = with pkgs.gnomeExtensions; [
    dash-to-dock
    places-status-indicator
    removable-drive-menu
    user-themes
    workspace-indicator
  ]
  ++ (optionals config.modules.i18n.japanese.enable [ kimpanel ]);
  extUuids = map (ext: ext.extensionUuid) exts
    ++ (optionals config.modules.desktop.apps.gnome.pomodoro.enable [ "pomodoro@arun.codito.in" ]);
in
{
  options.modules.desktop.gnome = with types; {
    enable = mkBoolOpt false;

    shell.theme = mkOpt (nullOr shellThemeType) null;
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

    user.packages = exts
      ++ [ (mkIf (cfg.shell.theme != null) cfg.shell.theme.package) ];

    modules.desktop.wayland.enable = true;

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "org/gnome/desktop/interface" = {
          enable-animations = false;
          # Disable Activities Overview hot corner
          enable-hot-corners = false;
        };
        "org/gnome/desktop/peripherals/keyboard" = {
          delay = 200;
          repeat = true;
          repeat-interval = 30;
        };
        "org/gnome/desktop/peripherals/mouse" = {
          accel-profile = "flat";
          speed = 0.7;
        };
        "org/gnome/desktop/wm/keybindings" = {
          switch-applications = [ ];
          switch-applications-backward = [ ];
          switch-windows = [ "<Super>Tab" ];
          switch-windows-backward = [ "<Shift><Super>Tab" ];
        };
        "org/gnome/shell/extensions" = {
          disable-user-extensions = false;
          enabled-extensions = extUuids;
        };
        "org/gnome/shell/extensions/dash-to-dock" = {
          dock-fixed = true;
        };
        "org/gnome/shell/window-switcher" = {
          current-workspace-only = false;
        };
      }
      // optionalAttrs (cfg.shell.theme != null) {
        "org/gnome/shell/extensions/user-theme" = {
          inherit (cfg.shell.theme) name;
        };
      };
    };

    modules.desktop.fontconfig.enable = true;
    modules.desktop.gtk.enable = true;
    modules.desktop.qt.enable = true;
  };
}
