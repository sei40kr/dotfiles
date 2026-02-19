{
  config,
  lib,
  osConfig,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib)
    attrByPath
    makeBinPath
    mkEnableOption
    mkIf
    ;

  cfg = config.modules.desktop.apps.quickshell;
in
{
  options.modules.desktop.apps.quickshell = {
    enable = mkEnableOption "Quickshell";
  };

  config = mkIf cfg.enable {
    programs.quickshell = {
      enable = true;
      configs.default = ./default;
      activeConfig = "default";
      systemd.enable = true;
    };

    systemd.user.services.quickshell.Service.Environment = [
      "PATH=${
        makeBinPath (
          with pkgs;
          [
            sysstat # mpstat for CPU usage
            procps # free for memory usage
            coreutils # df for disk usage
            glib # gdbus for Pomodoro timer
          ]
        )
      }"
    ];

    home.packages = mkIf (attrByPath [ "modules" "desktop" "wm" "niri" "enable" ] false osConfig) [
      perSystem.self.qml-niri
    ];
  };
}
