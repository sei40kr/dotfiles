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

  desktopCfg = config.modules.desktop;
  cfg = desktopCfg.gnome;
  inherit (desktopCfg) autoRepeat background fonts;

  exts = with pkgs.gnomeExtensions; [
    # blur-me
    dash-to-dock
    openweather
    places-status-indicator
    removable-drive-menu
    user-themes
    workspace-indicator
  ]
  ++ (optionals config.modules.i18n.japanese.enable [ kimpanel ]);
  extUuids = map (ext: ext.extensionUuid) exts
    ++ (optionals config.modules.desktop.apps.gnome.pomodoro.enable [ "pomodoro@arun.codito.in" ]);
  toPictureOpts = backgroundMode:
    if backgroundMode == "stretch" then "stretched"
    else if backgroundMode == "fill" then "scaled"
    else if backgroundMode == "fit" then "zoom"
    else if backgroundMode == "center" then "centered"
    else if backgroundMode == "tile" then "wallpaper"
    else throw "Unexpected background mode: ${backgroundMode}";
  pictureOpts = toPictureOpts background.image.mode;
in
{
  options.modules.desktop.gnome = with types; {
    enable = mkBoolOpt false;

    cursor.theme = mkOpt (nullOr cursorThemeType) null;
    shell.theme = mkOpt (nullOr shellThemeType) null;
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      desktopManager.gnome.enable = true;
    };
    qt.enable = mkForce false;

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
      pkgs.my.sensible-browser
      (mkIf (cfg.cursor.theme != null) cfg.cursor.theme.package)
      (mkIf (cfg.shell.theme != null) cfg.shell.theme.package)
    ] ++ exts;

    modules.desktop.enable = true;

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "org/gnome/desktop/interface" = {
          # Document font
          document-font-name = "${fonts.document.name} ${toString fonts.document.size}";
          enable-animations = false;
          # Disable Activities Overview hot corner
          enable-hot-corners = false;
          # Default font
          font-name = "${fonts.ui.name} ${toString fonts.ui.size}";
          # Monospace font
          monospace-font-name = "${fonts.fixed.name} ${toString fonts.fixed.size}";
        } // optionalAttrs (cfg.cursor.theme != null) {
          cursor-theme = cfg.cursor.theme.name;
        };
        "org/gnome/desktop/peripherals/keyboard" = {
          delay = autoRepeat.delay;
          repeat = true;
          repeat-interval = autoRepeat.interval;
        };
        "org/gnome/desktop/peripherals/mouse" = {
          accel-profile = "flat";
          speed = 0.7;
        };
        "org/gnome/desktop/screensaver" = ({
          primary-color = background.color;
        } // optionalAttrs (background.image != null) {
          picture-uri = "file://${background.image.path}";
          picture-options = pictureOpts;
        });
        "org/gnome/desktop/wm/keybindings" = {
          # Activate the window menu
          activate-window-menu = [ ];
          # Move window
          begin-move = [ ];
          # Resize window
          begin-resize = [ "<Super>r" ];
          # Close window
          close = [ "<Shift><Super>q" ];
          # Switch windows of an app directly
          cycle-group = [ ];
          cycle-group-backward = [ ];
          # Switch system controls directly
          cycle-panels = [ ];
          cycle-panels-backward = [ ];
          # Switch windows directly
          cycle-windows = [ ];
          cycle-windows-backward = [ ];
          # Maximize window
          maximize = [ "<Super>Up" "<Super>k" ];
          # Minimize window
          minimize = [ ];
          # Move window one monitor down
          move-to-monitor-down = [ "<Shift><Control><Super>Down" ];
          # Move window one monitor left
          move-to-monitor-left = [ "<Shift><Control><Super>Left" ];
          # Move window one monitor right
          move-to-monitor-right = [ "<Shift><Control><Super>Right" ];
          # Move window one monitor up
          move-to-monitor-up = [ "<Shift><Control><Super>Up" ];
          # Move window to workspace 1
          move-to-workspace-1 = [ "<Shift><Super>exclam" ];
          # Move window to workspace 2
          move-to-workspace-2 = [ "<Shift><Super>at" ];
          # Move window to workspace 3
          move-to-workspace-3 = [ "<Shift><Super>numbersign" ];
          # Move window to workspace 4
          move-to-workspace-4 = [ "<Shift><Super>dollar" ];
          # Move window to workspace 5
          move-to-workspace-5 = [ "<Shift><Super>percent" ];
          # Move window to workspace 6
          move-to-workspace-6 = [ "<Shift><Super>asciicircum" ];
          # Move window to workspace 7
          move-to-workspace-7 = [ "<Shift><Super>ampersand" ];
          # Move window to workspace 8
          move-to-workspace-8 = [ "<Shift><Super>asterisk" ];
          # Move window to workspace 9
          move-to-workspace-9 = [ "<Shift><Super>parenleft" ];
          # Move window to workspace 10
          move-to-workspace-10 = [ "<Shift><Super>parenright" ];
          # Move window one workspace down
          move-to-workspace-down = [ ];
          # Move window to last workspace
          move-to-workspace-last = [ ];
          # Move window one workspace to the left
          move-to-workspace-left = [ ];
          # Move window one workspace to the right
          move-to-workspace-right = [ ];
          # Move window one workspace up
          move-to-workspace-up = [ ];
          # Show the run command prompt
          panel-run-dialog = [ ];
          # Switch applications
          switch-applications = [ ];
          switch-applications-backward = [ ];
          # Switch to next input source
          switch-input-source = [ ];
          # Switch to previous input source
          switch-input-source-backward = [ ];
          # Switch system controls
          switch-panels = [ ];
          switch-panels-backward = [ ];
          # Switch to workspace 1
          switch-to-workspace-1 = [ "<Super>1" ];
          # Switch to workspace 2
          switch-to-workspace-2 = [ "<Super>2" ];
          # Switch to workspace 3
          switch-to-workspace-3 = [ "<Super>3" ];
          # Switch to workspace 4
          switch-to-workspace-4 = [ "<Super>4" ];
          # Switch to workspace 5
          switch-to-workspace-5 = [ "<Super>5" ];
          # Switch to workspace 6
          switch-to-workspace-6 = [ "<Super>6" ];
          # Switch to workspace 7
          switch-to-workspace-7 = [ "<Super>7" ];
          # Switch to workspace 8
          switch-to-workspace-8 = [ "<Super>8" ];
          # Switch to workspace 9
          switch-to-workspace-9 = [ "<Super>9" ];
          # Switch to workspace 10
          switch-to-workspace-10 = [ "<Super>0" ];
          # Switch to workspace 11
          switch-to-workspace-11 = [ "<Control><Super>1" ];
          # Switch to workspace 12
          switch-to-workspace-12 = [ "<Control><Super>2" ];
          # Switch to workspace down
          switch-to-workspace-down = [ ];
          # Switch to last workspace
          switch-to-workspace-last = [ ];
          # Switch to workspace left
          switch-to-workspace-left = [ "<Alt><Super>Left" ];
          # Switch to workspace right
          switch-to-workspace-right = [ "<Alt><Super>Right" ];
          # Switch to workspace up
          switch-to-workspace-up = [ ];
          # Switch windows
          switch-windows = [ "<Super>Tab" ];
          switch-windows-backward = [ "<Shift><Super>Tab" ];
          # Toggle fullscreen mode
          toggle-fullscreen = [ "<Super>f" ];
          # Toggle maximization state
          toggle-maximized = [ ];
          # Restore window
          unmaximize = [ "<Super>Down" "<Super>j" ];
        };
        "org/gnome/desktop/wm/preferences" = {
          # Use standard system font in window titles
          titlebar-uses-system-font = fonts.titlebar == null;
        } // optionalAttrs (fonts.titlebar != null) {
          # Window title font
          titlebar-font = "${fonts.titlebar.name} ${toString fonts.titlebar.size}";
        };
        "org/gnome/mutter/keybindings" = {
          # View split on left
          toggle-tiled-left = [ "<Super>Left" "<Super>h" ];
          # View split on right
          toggle-tiled-right = [ "<Super>Right" "<Super>l" ];
        };
        "org/gnome/settings-daemon/plugins/media-keys/help" = {
          # Settings
          control-center = [ "<Super>c" ];
          # Eject
          eject = [ "Eject" ];
          # Launch help browser
          help = [ ];
          # Home folder
          home = [ "<Shift><Super>n" ];
          # Log out
          logout = [ "<Shift><Super>e" ];
          # Lock screen
          screensaver = [ ];
          # Search
          search = [ "<Alt><Super>space" ];
          # Volume down
          volume-down = [ "AudioLowerVolume" ];
          # Volume mute/unmute
          volume-mute = [ "AudioMute" ];
          # Volume up
          volume-up = [ "AudioRaiseVolume" ];
          # Launch web browser
          www = [ "<Shift><Super>Return" ];
        };
        "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
          binding = "<Super>Return";
          command = "sensible-terminal";
          name = "Launch terminal";
        };
        "org/gnome/settings-daemon/plugins/power" = {
          power-button-action = "suspend";
          sleep-inactive-ac-type = "nothing";
        };
        "org/gnome/shell" = {
          favorite-apps =
            optionals config.modules.desktop.apps.gnome.nautilus.enable [ "org.gnome.Nautilus.desktop" ] ++
              optionals config.modules.desktop.apps.thunar.enable [ "thunar.desktop" ] ++
              optionals config.modules.desktop.browsers.chrome.enable [ "google-chrome.desktop" ] ++
              optionals config.modules.desktop.browsers.firefox.enable [ "firefox.desktop" ] ++
              optionals config.modules.desktop.apps.gnome.geary.enable [ "org.gnome.Geary.desktop" ] ++
              optionals config.modules.desktop.browsers.chrome.webapps.gmail.enable [ "web-gmail.desktop" ] ++
              optionals config.modules.desktop.browsers.chrome.webapps.google-maps.enable [ "web-google-maps.desktop" ] ++
              optionals config.modules.desktop.browsers.chrome.webapps.google-photos.enable [ "web-google-photos.desktop" ] ++
              optionals config.modules.desktop.apps.gnome.calendar.enable [ "org.gnome.Calendar.desktop" ] ++
              optionals config.modules.desktop.browsers.chrome.webapps.google-calendar.enable [ "web-google-calendar.desktop" ] ++
              optionals config.modules.desktop.apps.gnome.todo.enable [ "org.gnome.Todo.desktop" ] ++
              optionals config.modules.desktop.browsers.chrome.webapps.youtube-music.enable [ "web-youtube-music.desktop" ] ++
              optionals config.modules.desktop.apps.todoist.enable [ "todoist-electron.desktop" ] ++
              optionals config.modules.desktop.browsers.chrome.webapps.notion.enable [ "web-notion.desktop" ] ++
              optionals config.modules.desktop.apps.slack.enable [ "slack.desktop" ] ++
              optionals config.modules.desktop.apps.element.enable [ "element-desktop.desktop" ] ++
              optionals config.modules.desktop.apps.discord.enable [ "discord.desktop" ] ++
              optionals config.modules.desktop.apps.zoom.enable [ "Zoom.desktop" ] ++
              optionals config.modules.desktop.apps.qbittorrent.enable [ "org.qbittorrent.qBittorrent.desktop" ] ++
              optionals config.modules.term.gnome.enable [ "org.gnome.Terminal.desktop" ] ++
              optionals config.modules.term.alacritty.enable [ "Alacritty.desktop" ] ++
              optionals config.modules.term.kitty.enable [ "kitty.desktop" ] ++
              optionals config.modules.editors.nvim.enable [ "yvim-qt.desktop" ] ++
              optionals config.modules.editors.emacs.enable [ "emacs.desktop" ] ++
              optionals config.modules.editors.idea.enable [ "idea-ultimate.desktop" ] ++
              optionals config.modules.editors.datagrip.enable [ "datagrip.desktop" ] ++
              optionals config.modules.editors.dataspell.enable [ "dataspell.desktop" ] ++
              optionals config.modules.desktop.apps.zeal.enable [ "org.zealdocs.zeal.desktop" ] ++
              [ "org.gnome.Settings.desktop" "ca.desrt.dconf-editor.desktop" ];
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
          # Disable keyboard shortcuts to activate apps
          hot-keys = false;
          # Hide "Show Applications" button
          show-show-apps-button = true;
        };
        # OpenWeather
        "org/gnome/shell/extensions/openweather" = {
          # Position In Panel
          position-in-panel = "center";
          # Position Offset
          position-index = 1;
          # Hide conditions in forecast
          show-comment-in-forecast = false;
          # Keep forecast expanded
          expand-forecast = true;
          # Locations
          city = "33.6752943,130.9814491>京都郡, 福岡県, 800-0334, 日本>0";
        };
        "org/gnome/shell/keybindings" = {
          # Focus the active notification
          focus-active-notification = [ ];
          # Open the application menu
          open-application-menu = [ ];
          # Switch to application 1
          switch-to-application-1 = [ ];
          # Switch to application 2
          switch-to-application-2 = [ ];
          # Switch to application 3
          switch-to-application-3 = [ ];
          # Switch to application 4
          switch-to-application-4 = [ ];
          # Switch to application 5
          switch-to-application-5 = [ ];
          # Switch to application 6
          switch-to-application-6 = [ ];
          # Switch to application 7
          switch-to-application-7 = [ ];
          # Switch to application 8
          switch-to-application-8 = [ ];
          # Switch to application 9
          switch-to-application-9 = [ ];
          # Show all applications
          toggle-application-view = [ ];
          # Show the notification list
          toggle-message-tray = [ "<Super>n" ];
          # Show the overview
          toggle-overview = [ ];
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
      // optionalAttrs (background.image != null) {
        "org/gnome/desktop/background" = {
          picture-uri = "file://${background.image.path}";
          picture-options = pictureOpts;
        };
      };
    };

    modules.desktop.fontconfig.enable = true;
    modules.desktop.gtk.enable = true;
    modules.desktop.qt.enable = true;

    modules.term.sensible.enable = true;
  };
}
