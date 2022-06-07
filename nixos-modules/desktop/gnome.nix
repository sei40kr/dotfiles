{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cursorThemeType = with types; submodule {
    options = {
      package = mkOpt package null;
      name = mkOpt str null;
    };
  };
  shellThemeType = with types; submodule {
    options = {
      package = mkOpt package null;
      name = mkOpt str null;
    };
  };

  backgroundType = with types; submodule {
    options = {
      image = mkOpt (either str path) null;
      mode = mkOpt (enum [ "none" "wallpaper" "centered" "scaled" "stretched" "zoom" "spanned" ]) "zoom";
    };
  };

  cfg = config.modules.desktop.gnome;

  exts = with pkgs.gnomeExtensions; [
    # blur-me
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

    cursor.theme = mkOpt (nullOr cursorThemeType) null;
    shell.theme = mkOpt (nullOr shellThemeType) null;

    background = mkOpt (nullOr backgroundType) null;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      desktopManager.gnome.enable = true;
    };

    programs.gpaste.enable = true;

    environment.gnome.excludePackages = with pkgs.gnome; [
      baobab
      cheese
      eog
      epiphany
      gedit
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

    user.packages = [
      pkgs.gnome.dconf-editor
      (mkIf (cfg.cursor.theme != null) cfg.cursor.theme.package)
      (mkIf (cfg.shell.theme != null) cfg.shell.theme.package)
    ] ++ exts;

    modules.desktop = {
      enable = true;
      wayland = true;
    };

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "org/gnome/desktop/interface" = {
          enable-animations = false;
          # Disable Activities Overview hot corner
          enable-hot-corners = false;
        } // optionalAttrs (cfg.cursor.theme != null) {
          cursor-theme = cfg.cursor.theme.name;
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
        } // optionalAttrs config.modules.editors.emacs.doom.enable {
          activate-window-menu = [ ];
        };
        "org/gnome/shell/extensions" = {
          disable-user-extensions = false;
          enabled-extensions = extUuids;
        };
        # "org/gnome/shell/extensions/blur-me" = {
        #   toggle-app-blur = false;
        # };
        "org/gnome/shell/extensions/dash-to-dock" = {
          dock-fixed = true;
          # Hide "Show Applications" button
          show-show-apps-button = true;
        };
        "org/gnome/shell/window-switcher" = {
          current-workspace-only = false;
        };
      }
      // optionalAttrs (cfg.shell.theme != null) {
        "org/gnome/shell/extensions/user-theme" = {
          inherit (cfg.shell.theme) name;
        };
      }
      // optionalAttrs (cfg.background != null) {
        "org/gnome/desktop/background" = {
          picture-uri = "file://${cfg.background.image}";
          picture-options = cfg.background.mode;
        };
      };
    };

    modules.desktop.fontconfig.enable = true;
    modules.desktop.gtk.enable = true;
    modules.desktop.qt.enable = true;
  };
}
