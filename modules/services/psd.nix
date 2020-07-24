{ config, lib, options, pkgs, ... }:

with lib;
(let cfg = config.modules.services.psd;
in {
  options.modules.services.psd = {
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
            Description = "Profile-sync-daemon";
            Documentation = [
              "man:psd(1)"
              "man:profile-sync-daemon(1)"
              "https://wiki.archlinux.org/index.php/Profile-sync-daemon"
            ];
            Wants = [ "psd-resync.service" ];
            RequiresMountsFor = [ "/home/" ];
            After = [ "winbindd.service" ];
          };
          Service = {
            Type = "oneshot";
            RemainAfterExit = "yes";
            # just call /bin/true and let psd-resync.service do it
            ExecStart = "${pkgs.coreutils}/bin/true";
            ExecStop =
              "${pkgs.profile-sync-daemon}/bin/profile-sync-daemon unsync";
          };
          Install.WantedBy = [ "default.target" ];
        };
        psd-resync = {
          Unit = {
            Description = "Timed resync";
            After = [ "psd.service" ];
            Wants = [ "psd-resync.timer" ];
            PartOf = [ "psd.service" ];
          };
          Service = {
            Type = "oneshot";
            ExecStart =
              "${pkgs.profile-sync-daemon}/bin/profile-sync-daemon resync";
          };
          Install.WantedBy = [ "default.target" ];
        };
      };
      timers.psd-resync = {
        Unit = {
          Description = "Timer for profile-sync-daemon - ${cfg.resyncTimer}";
          PartOf = [ "psd-resync.service" "psd.service" ];
        };
        Timer.OnUnitActiveSec = cfg.resyncTimer;
      };
    };
  };
})
