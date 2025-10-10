{ config, lib, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.services.ssh;
in
{
  options.modules.services.ssh = {
    enable = mkBoolOpt false;
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

    user.openssh.authorizedKeys.keys =
      if config.user.name == "sei40kr" then
        [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK94YRijT3xT+bu3fhfg41Ieu++1VKkqg0xv2mr+hV7C sei40krs-iPad" ]
      else
        [ ];
  };
}
