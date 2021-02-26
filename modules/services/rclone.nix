{ config, home-manager, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.rclone;
  remotes = (optionals cfg.enableGooglePhotos [ "google-photos" ])
    ++ (optionals cfg.enableGoogleDrive [ "google-drive" ]);
in {
  options.modules.services.rclone = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableGooglePhotos = mkOption {
      type = types.bool;
      default = false;
    };

    enableGoogleDrive = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rclone ];
    home-manager.users.${config.user.name}.systemd.user.services =
      builtins.listToAttrs (map (remote:
        nameValuePair "rclone-${remote}" {
          Unit = {
            Description =
              "RClone mount of users remote ${remote} using filesystem permissions";
            Documentation = "http://rclone.org/docs";
            After = [ "network-online.target" ];
            X-Restart-Triggers = [ "%h/.config/rclone/rclone.conf" ];
          };
          Service = {
            Type = "notify";
            ExecStartPre = ''
              ${escapeShellArg "${pkgs.coreutils}/bin/mkdir"} -p "$MOUNT_DIR"
            '';
            ExecStart = ''
              ${escapeShellArg "${pkgs.rclone}/bin/rclone"} mount \
                ${escapeShellArg "${remote}:/"} "$MOUNT_DIR"
            '';
            ExecStop = ''
              ${escapeShellArg "${config.security.wrapperDir}/fusermount"} -u \
                "$MOUNT_DIR"
            '';
            Restart = "on-success";
            RestartSec = 10;
            Environment = [ "MOUNT_DIR=%h/${remote}" ];
          };
          Install.WantedBy = [ "default.target" ];
        }) remotes);
  };
}
