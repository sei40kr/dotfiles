{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.services.rclone;
in {
  options.modules.services.rclone = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    remotesToAutoMount = mkOption {
      type = with types; listOf str;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ rclone ];
    my.home.systemd.user.services = mkMerge (map (remote:
      let mountDir = "%h/${remote}";
      in {
        "rclone-${remote}" = {
          Unit = {
            Description =
              "RClone mount of users remote ${remote} using filesystem permissions";
            Documentation = "http://rclone.org/docs";
            After = [ "network-online.target" ];
            X-Restart-Triggers = [ "%h/.config/rclone/rclone.conf" ];
          };
          Service = {
            Type = "notify";
            ExecStartPre =
              "${pkgs.coreutils}/bin/mkdir -p ${escapeShellArg mountDir}";
            ExecStart = ''
              ${pkgs.rclone}/bin/rclone mount \
                                        ${escapeShellArg "${remote}:/"} \
                                        ${escapeShellArg mountDir}
            '';
            ExecStop = "${config.security.wrapperDir}/fusermount -u ${
                escapeShellArg mountDir
              }";
            Restart = "on-success";
            RestartSec = 10;
          };
          Install.WantedBy = [ "default.target" ];
        };
      }) cfg.remotesToAutoMount);
  };
}
