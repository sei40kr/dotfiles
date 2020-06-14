{ config, lib, options, pkgs, ... }:

with lib;
(let cfg = config.modules.desktop.tools.psd;
in {
  options.modules.desktop.tools.psd = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    resyncTimer = mkOption {
      type = types.str;
      default = "1h";
      example = "1h 30min";
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ profile-sync-daemon ];

    my.home.systemd.user = {
      services = {
        psd = {
          Unit = {
            Description = "Profile Sync daemon";
            Wants = [ "psd-resync.service" ];
            RequiresMountsFor = [ "/home/" ];
          };
          Service = {
            Type = "oneshot";
            RemainAfterExit = "yes";
            ExecStart =
              "${pkgs.profile-sync-daemon}/bin/profile-sync-daemon sync";
            ExecStop =
              "${pkgs.profile-sync-daemon}/bin/profile-sync-daemon unsync";
          };
          Install.WantedBy = [ "default.target" ];
        };
        psd-resync = {
          Unit = {
            Description = "Timed profile resync";
            Wants = [ "psd-resync.timer" ];
            After = [ "psd.service" ];
            PartOf = [ "psd.service" ];
          };
          Install = { WantedBy = [ "default.target" ]; };
          Service = {
            Type = "oneshot";
            ExecStart =
              "${pkgs.profile-sync-daemon}/bin/profile-sync-daemon resync";
          };
        };
      };
      timers.psd-resync = {
        Unit = {
          Description = "Timer for profile sync daemon - ${cfg.resyncTimer}";
          PartOf = [ "psd-resync.service" "psd.service" ];
        };
        Timer = { OnUnitActiveSec = "${cfg.resyncTimer}"; };
      };
    };
  };
})
