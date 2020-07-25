{ config, lib, pkgs, ... }:

with lib; {
  options.modules.services.jellyfin.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.jellyfin.enable {
    my.packages = with pkgs; [ jellyfin ];
    my.home.systemd.user.services.jellyfin = {
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
