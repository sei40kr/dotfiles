{ config, lib, pkgs, ... }:

with lib; {
  options.modules.services.protonvpn.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.protonvpn.enable {
    my.packages = with pkgs.my; [ protonvpn-cli-wrapper ];
    my.aliases = {
      protonvpn = "sudo protonvpn";
      pvpn = "sudo protonvpn";
    };
    systemd.services.protonvpn-autoconnect = {
      wantedBy = [ "multi-user.target" ];
      description = "ProtonVPN-CLI auto-connect";
      wants = [ "network-online.target" ];
      serviceConfig = {
        Type = "forking";
        Environment =
          [ "PVPN_WAIT=300" "PVPN_DEBUG=1" "SUDO_USER=${config.my.userName}" ];
        ExecStart = "${pkgs.my.protonvpn-cli-wrapper}/bin/protonvpn c --p2p";
      };
    };
  };
}
