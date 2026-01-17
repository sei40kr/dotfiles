{
  config,
  lib,
  osConfig,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.desktop.apps.quickshell;
in
{
  options.modules.desktop.apps.quickshell = {
    enable = mkEnableOption "Quickshell";
  };

  config = mkIf cfg.enable {
    programs.quickshell = {
      enable = true;
      configs.default = ../../config/quickshell;
      activeConfig = "default";
      systemd.enable = true;
    };

    systemd.user.services.quickshell.Service.Environment = [
      "PATH=${
        lib.makeBinPath (
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

    home.packages = mkIf osConfig.modules.desktop.wm.niri.enable [ perSystem.self.qml-niri ];
  };
}
