{
  config,
  lib,
  inputs,
  osConfig,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) attrByPath mkEnableOption mkIf;

  cfg = config.modules.desktop.apps.noctalia-shell;

  deCfg = attrByPath [ "modules" "desktop" "de" ] { } osConfig;
  bgImage = attrByPath [ "background" "image" ] null deCfg;
  bgColor = attrByPath [ "background" "color" ] "#000000" deCfg;

  fillModeMap = {
    fill = "crop";
    fit = "fit";
    center = "center";
    stretch = "stretch";
    tile = "repeat";
  };

  messageSound = "${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/bell.oga";

  niriEnabled = attrByPath [ "modules" "desktop" "wm" "niri" "enable" ] false osConfig;
in
{
  imports = [ inputs.noctalia-shell.homeModules.default ];

  options.modules.desktop.apps.noctalia-shell = {
    enable = mkEnableOption "Noctalia Shell";
  };

  config = mkIf cfg.enable {
    programs.noctalia-shell = {
      enable = true;
      package = perSystem.noctalia-shell.default;
      settings = {
        bar = {
          position = "top";
          density = "spacious";
          widgets = {
            left = [ { id = "Workspace"; } ];
            center = [
              {
                id = "ActiveWindow";
                # Widen the default cap (screen.width * 0.06); grows with content.
                maxWidth = 480;
              }
            ];
            right = [
              { id = "MediaMini"; }
              { id = "Tray"; }
              { id = "NotificationHistory"; }
              { id = "ControlCenter"; }
              {
                id = "Clock";
                formatHorizontal = "MMM d ddd  hh:mm";
              }
              { id = "SessionMenu"; }
            ];
          };
        };
        general = {
          showScreenCorners = true;
          shadowDirection = "center";
          shadowOffsetX = 0;
          shadowOffsetY = 0;
        };
        ui = {
          fontDefault = deCfg.defaultFonts.ui.name;
          fontFixed = deCfg.defaultFonts.fixed.name;
          settingsPanelMode = "centered";
          settingsPanelSideBarCardStyle = true;
        };
        location.autoLocate = true;
        wallpaper =
          (
            if bgImage != null then
              {
                enabled = true;
                automationEnabled = false;
                useSolidColor = false;
                setWallpaperOnAllMonitors = true;
                fillMode = fillModeMap.${bgImage.mode} or "crop";
                directory = builtins.dirOf bgImage.path;
              }
            else
              {
                enabled = true;
                useSolidColor = true;
                solidColor = bgColor;
              }
          )
          // {
            transitionType = [ "fade" ];
          };
        appLauncher.enableClipboardHistory = true;
        dock.enabled = false;
        notifications.sounds = {
          enabled = true;
          normalSoundFile = messageSound;
          criticalSoundFile = messageSound;
          lowSoundFile = messageSound;
        };
        colorSchemes = {
          useWallpaperColors = false;
          predefinedScheme = "Tokyo Night";
          darkMode = true;
        };
        # Generate config snippets for GTK and Qt applications from the active
        # color scheme so they match the shell.
        # https://docs.noctalia.dev/v4/theming/program-specific/gtk-qt/
        templates.activeTemplates = [
          {
            id = "gtk";
            enabled = true;
          }
          {
            id = "qt";
            enabled = true;
          }
        ];
        nightLight = {
          enabled = true;
          autoSchedule = true;
          dayTemp = "5500";
          nightTemp = "3700";
        };
        idle = {
          enabled = true;
          # Never turn the screen off (DPMS) or suspend on idle; keep the lock stage.
          screenOffTimeout = 0;
          suspendTimeout = 0;
        };
      };
    };

    # Application-side wiring for Noctalia's GTK/Qt templates.
    # https://docs.noctalia.dev/v4/theming/program-specific/gtk-qt/
    #
    # GTK apps recolor by importing Noctalia's generated CSS on top of a neutral
    # base theme (adw-gtk3); Qt apps read the generated palette through qt6ct.
    gtk = {
      enable = true;
      theme = {
        package = pkgs.adw-gtk3;
        name = "adw-gtk3";
      };
      # adw-gtk3 is GTK3-only; GTK4 apps stay on libadwaita and get recolored by
      # Noctalia's generated gtk-4.0 CSS instead (per the Noctalia docs).
      gtk4.theme = null;
      # Also drives the gsettings `org.gnome.desktop.interface font-name` key,
      # which Chromium-based apps (e.g. Microsoft Edge) read for their UI font
      # since they don't follow gtk-font-name.
      font = {
        inherit (deCfg.defaultFonts.ui) package name size;
      };
    };

    qt = {
      enable = true;
      platformTheme.name = "qtct";
    };

    # Select the palette Noctalia generates for the "qt" template.
    xdg.configFile."qt6ct/qt6ct.conf".text = ''
      [Appearance]
      style=Fusion
      custom_palette=true
      color_scheme_path=${config.home.homeDirectory}/.config/qt6ct/colors/noctalia.conf
      standard_dialogs=default
    '';

    home.file.".cache/noctalia/wallpapers.json" = mkIf (bgImage != null) {
      force = true;
      text = builtins.toJSON {
        wallpapers = { };
        defaultWallpaper = bgImage.path;
        usedRandomWallpapers = { };
      };
    };

    xdg.configFile."niri/config.kdl".text = mkIf niriEnabled ''
      include "/etc/niri/config.kdl"
      spawn-at-startup "noctalia-shell"

      environment {
          QT_QPA_PLATFORMTHEME "qt6ct"
      }
    '';
  };
}
