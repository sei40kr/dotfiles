{ config, lib, ... }:

with lib;
(let cfg = config.modules.services.sshd;
in {
  options.modules.services.sshd = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableFail2ban = mkOption {
      type = types.bool;
      default = true;
    };

    enableGoogleAuthenticator = mkOption {
      type = types.bool;
      default = true;
    };
  };

  config = {
    services.openssh = {
      enable = cfg.enable;
      permitRootLogin = "no";
    };

    # Enable fail2ban
    services.fail2ban.enable = cfg.enableFail2ban;

    # Enable Google Authenticator
    security.pam.services.sshd.googleAuthenticator.enable =
      cfg.enableGoogleAuthenticator;
  };
})
