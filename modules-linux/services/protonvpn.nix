{ config, lib, pkgs, ... }:

with lib; {
  options.modules.services.protonvpn.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.services.protonvpn.enable {
    user.packages = with pkgs; [ protonvpn-cli ];
    systemd.services.protonvpn-autoconnect = {
      wantedBy = [ "multi-user.target" ];
      description = "ProtonVPN-CLI auto-connect";
      wants = [ "network-online.target" ];
      serviceConfig = {
        Type = "forking";
        Environment =
          [ "PVPN_WAIT=300" "PVPN_DEBUG=1" "SUDO_USER=${config.user.name}" ];
        ExecStart = "${pkgs.protonvpn-cli}/bin/protonvpn c --p2p";
      };
    };
    modules.shell.aliases = {
      protonvpn = "sudo protonvpn";
      pvpn = "sudo protonvpn";
    };
  };
}
