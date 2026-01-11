{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption;

  desktopCfg = config.modules.desktop;
  cfg = desktopCfg.apps.quickshell;
in
{
  options.modules.desktop.apps.quickshell = {
    enable = mkEnableOption "Quickshell";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      quickshell
      (mkIf desktopCfg.wm.niri.enable my.qml-niri)
    ];

    home.configFile."quickshell".source = ../../../config/quickshell;

    systemd.user.services.quickshell = {
      description = "Quickshell - Flexible toolkit for making desktop shells with QtQuick, for Wayland and X11";
      documentation = [ "https://github.com/outfoxxed/quickshell" ];
      partOf = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      requisite = [ "graphical-session.target" ];
      # Commands used by status bar indicators
      path = with pkgs; [
        sysstat # mpstat for CPU usage
        procps # free for memory usage
        coreutils # df for disk usage
        glib # gdbus for Pomodoro timer
      ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.quickshell}/bin/quickshell";
        Restart = "on-failure";
      };
      wantedBy = [ "graphical-session.target" ];
    };

    # Necessary to set QML2_IMPORT_PATH
    qt.enable = true;
  };
}
