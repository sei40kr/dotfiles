{ config, home-manager, lib, pkgs, ... }:

with lib;
let cfg = config.modules.services.jellyfin;
in {
  options.modules.services.jellyfin = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    openFirewall = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    # cf https://jellyfin.org/docs/general/networking/index.html
    networking.firewall.allowedTCPPorts =
      optionals cfg.openFirewall [ 8096 8920 ];

    user.packages = with pkgs; [ jellyfin ];
    home-manager.users.${config.user.name}.systemd.user.services.jellyfin = {
      Unit = {
        Description = "Jellyfin Media Server";
        After = [ "network.target" ];
      };
      Service = {
        ExecStart = "${pkgs.jellyfin}/bin/jellyfin";
        Restart = "on-failure";
      };
    };
  };
}
