{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.services.ssh;
in
{
  options.modules.services.ssh = {
    enable = mkEnableOption "OpenSSH server";
  };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };

    users.users.sei40kr.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK94YRijT3xT+bu3fhfg41Ieu++1VKkqg0xv2mr+hV7C sei40krs-iPad"
    ];
  };
}
