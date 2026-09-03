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

  cfg = config.modules.desktop.apps.noctalia;

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
  imports = [ inputs.noctalia.homeModules.default ];

  options.modules.desktop.apps.noctalia = {
    enable = mkEnableOption "Noctalia Shell";
  };

  config = mkIf cfg.enable {
    programs.noctalia = {
      enable = true;
      package = perSystem.noctalia.default;
      settings = {
        bar.default = {
          position = "top";
          # Match v4's "spacious" density where it exceeds the v5 defaults:
          # bar height 47 (default 34) and ~16px font (14px base * 1.15).
          # accessibility.ui_scale does not cover the bar.
          thickness = 47;
          scale = 1.15;
          start = [ "workspaces" ];
          center = [ "active_window" ];
          end = [
            "media"
            "tray"
            "notifications"
            "control-center"
            "clock"
            "session"
          ];
        };
        widget = {
          # Widen the default cap; grows with content.
          active_window.max_length = 480;
          clock.format = "{:%b %-d %a  %H:%M}";
        };
        # Global UI scale (0.5–2.5); v5's defaults render smaller than v4 did.
        accessibility.ui_scale = 1.2;
        shell = {
          font_family = deCfg.defaultFonts.ui.name;
          # Skip the first-run setup wizard; everything it configures is
          # declared here.
          setup_wizard_enabled = false;
          screen_corners.enabled = true;
          shadow.direction = "center";
          clipboard_enabled = true;
          # Freeform command runner for the Mod+Shift+Space keybind (see
          # modules/nixos/niri/config.kdl); v5 has no built-in launcher
          # command mode.
          launcher.dmenu.entry.run = {
            label = "Run";
            prefix = "/run";
            freeform = true;
            exec = "{query}";
          };
        };
        location.auto_locate = true;
        wallpaper =
          if bgImage != null then
            {
              enabled = true;
              fill_mode = fillModeMap.${bgImage.mode} or "crop";
              directory = builtins.dirOf bgImage.path;
              default.path = bgImage.path;
              transition = [ "fade" ];
            }
          else
            {
              enabled = true;
              default.path = "color:${bgColor}";
              transition = [ "fade" ];
            };
        dock.enabled = false;
        audio = {
          enable_sounds = true;
          # v5 has a single notification sound; the v4 per-urgency files are
          # gone.
          notification_sound = messageSound;
        };
        theme = {
          mode = "dark";
          source = "builtin";
          builtin = "Tokyo-Night";
          # Generate config snippets for GTK and Qt applications from the
          # active color scheme so they match the shell.
          # https://docs.noctalia.dev/theming/app-theming/
          templates = {
            enable_builtin_templates = true;
            builtin_ids = [
              "gtk3"
              "gtk4"
              "qt"
            ];
          };
        };
        nightlight = {
          enabled = true;
          # force = false (default) follows the [location] schedule.
          temperature_day = 5500;
          temperature_night = 3700;
        };
        # Keep the lock stage (v4 default: 11 min); screen-off (DPMS) and
        # suspend behaviors stay at their disabled defaults.
        idle.behavior.lock = {
          enabled = true;
          timeout = 660;
          action = "lock";
        };
      };
    };

    # Application-side wiring for Noctalia's GTK/Qt templates.
    # https://docs.noctalia.dev/theming/app-theming/
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

    xdg.configFile."niri/config.kdl".text = mkIf niriEnabled ''
      include "/etc/niri/config.kdl"
      spawn-at-startup "noctalia"

      environment {
          QT_QPA_PLATFORMTHEME "qt6ct"
      }
    '';
  };
}
