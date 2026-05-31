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
  termCfg = config.modules.term;

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

  noctaliaTheme = termCfg.colorschemes.themes.${termCfg.colorschemes.active};

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
          predefinedScheme = noctaliaTheme.noctalia;
          darkMode = noctaliaTheme.noctaliaDarkMode;
        };
        nightLight = {
          enabled = true;
          autoSchedule = true;
          dayTemp = "5500";
          nightTemp = "3700";
        };
        idle.enabled = true;
      };
    };

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
    '';
  };
}
