{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins)
    attrValues
    concatStringsSep
    elem
    mapAttrs
    ;
  inherit (lib)
    filterAttrs
    mdDoc
    mkEnableOption
    mkIf
    optionalString
    removePrefix
    ;

  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  wmCfg = desktopCfg.wm;
  cfg = wmCfg.hyprland;

  package = pkgs.hyprland;

  toMonitorParams =
    { enable, scale, ... }@monitor:
    let
      resolution =
        if monitor.resolution != null then
          (toString monitor.resolution.width)
          + "x"
          + (toString monitor.resolution.height)
          + (optionalString (monitor.refreshRate != null) "@" + monitor.refreshRate)
        else
          "preferred";
      position =
        if monitor.position != null then
          (toString monitor.position.x) + "x" + (toString monitor.position.y)
        else
          "auto";
      mirror = optionalString (monitor.mirror != null) ", mirror, ${monitor.mirror}";
      bitDepth = optionalString (monitor.bitDepth != 8) ", bit_depth, ${toString monitor.bitDepth}";
      vrr = optionalString monitor.vrr ", vrr, 1";
      transform =
        ", transform, "
        + (
          if monitor.rotation.degrees == 0 && !monitor.rotation.flipped then
            "0"
          else if monitor.rotation.degrees == 90 && !monitor.rotation.flipped then
            "1"
          else if monitor.rotation.degrees == 180 && !monitor.rotation.flipped then
            "2"
          else if monitor.rotation.degrees == 270 && !monitor.rotation.flipped then
            "3"
          else if monitor.rotation.degrees == 0 && monitor.rotation.flipped then
            "4"
          else if monitor.rotation.degrees == 90 && monitor.rotation.flipped then
            "5"
          else if monitor.rotation.degrees == 180 && monitor.rotation.flipped then
            "6"
          else if monitor.rotation.degrees == 270 && monitor.rotation.flipped then
            "7"
          else
            throw "Invalid rotation"
        );
    in
    if enable then
      "${resolution}, ${position}, ${toString scale}${transform}${mirror}${bitDepth}${vrr}"
    else
      "disable";

  embeddedMonitorParams =
    if deCfg.monitors ? eDP-1 then toMonitorParams deCfg.monitors.eDP-1 else "preferred, auto, 1";

  monitors-conf = pkgs.writeTextFile {
    name = "monitors.conf";
    text = concatStringsSep "\n" (
      attrValues (
        mapAttrs (name: monitor: "monitor = ${name}, ${toMonitorParams monitor}") (
          filterAttrs (k: _: k != "eDP-1") deCfg.monitors
        )
      )
    );
  };

  sync-lid-state = pkgs.writeShellScriptBin "hyprland-sync-lid-state" ''
    if ${pkgs.gnugrep}/bin/grep -q open /proc/acpi/button/lid/LID/state; then
      ${package}/bin/hyprctl keyword monitor "eDP-1, ${embeddedMonitorParams}"
    else
      ${package}/bin/hyprctl keyword monitor "eDP-1, disable"
    fi
  '';
in
{
  options.modules.desktop.wm.hyprland = {
    enable = mkEnableOption (mdDoc ''
      Whether to enable Hyprland.
    '');
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion =
          deCfg.background.image == null
          || elem deCfg.background.image.mode [
            "fill"
            "fit"
          ];
        message = "The background image mode must be either \"fill\" or \"fit\" when using Hyprland";
      }
    ];

    user.packages = with pkgs; [ hyprpaper ];

    programs.hyprland = {
      inherit package;
      enable = true;
    };

    home.configFile."hypr/hyprland.conf".text = ''
      general {
        gaps_in = ${toString wmCfg.gaps.inner}
        gaps_out = ${toString wmCfg.gaps.outer}
      }

      animations {
        enabled = false
      }

      input {
        repeat_rate = ${toString deCfg.autoRepeat.interval}
        repeat_delay = ${toString deCfg.autoRepeat.delay}
      }

      misc {
        background_color = 0x${removePrefix "#" deCfg.background.color}
      }

      source = ${monitors-conf}

      exec-once = ${sync-lid-state}/bin/hyprland-sync-lid-state
      bindl=, switch:Lid Switch, exec, ${sync-lid-state}/bin/hyprland-sync-lid-state

      source = ${../../../config/hyprland/hyprland.conf}

      env = WLR_NO_HARDWARE_CURSORS,1

      ${optionalString (deCfg.background.image != null) ''
        exec-once = hyprpaper &
      ''}
      exec = systemctl --user start swayidle.service &
      exec = systemctl --user start waybar.service &
      ${optionalString config.modules.i18n.japanese.enable ''
        exec = systemctl --user start fcitx5-daemon.service &
      ''}
    '';

    programs.hyprlock.enable = true;

    home.configFile."hypr/hyprpaper.conf" = mkIf (deCfg.background.image != null) {
      text = ''
        preload = ${deCfg.background.image.path}
        wallpaper = ,${
          optionalString (deCfg.background.image.mode == "fit") "contain:"
        }${deCfg.background.image.path}
        ipc = off
      '';
    };

    modules.desktop.de.enable = true;
    modules.desktop.de.wayland = true;

    modules.desktop.browsers.sensible.enable = true;
    modules.term.sensible.enable = true;
    modules.desktop.apps.dunst.enable = true;
    modules.desktop.apps.rofi.enable = true;
    modules.desktop.apps.waybar.enable = true;

    modules.desktop.dconf.enable = true;
    modules.desktop.fontconfig.enable = true;
    modules.desktop.gtk.enable = true;
    modules.desktop.qt.enable = true;
  };
}
