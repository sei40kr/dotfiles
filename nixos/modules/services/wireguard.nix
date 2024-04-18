{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.services.wireguard;
in
{
  options.modules.services.wireguard = {
    enable = mkEnableOption "WireGuard";
  };

  config = mkIf cfg.enable {
    age.secrets.wg0-private.file = ../../../config/wireguard/wg0/private.age;
    age.secrets.wg0-peer1-psk.file = ../../../config/wireguard/wg0/peer1-psk.age;

    networking.wg-quick.interfaces.wg0 = {
      address = [ "100.118.32.138/32" ];
      autostart = false;
      dns = [ "10.255.255.2" ];
      privateKeyFile = config.age.secrets.wg0-private.path;
      listenPort = 51820;

      peers = [
        # Tokyo Shinkansen
        {
          publicKey = "8n68GM7n6dm6Hj3RIIh5q1q6Un52Cq82LYEXHRAtPg4=";
          allowedIPs = [ "0.0.0.0/0" "::/0" ];
          endpoint = "hnd-148-wg.whiskergalaxy.com:65142";
          presharedKeyFile = config.age.secrets.wg0-peer1-psk.path;
        }
      ];
    };

    networking.firewall.allowedUDPPorts = [ 51820 ];
  };
}
