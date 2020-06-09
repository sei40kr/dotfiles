{ config, lib, options, pkgs, ... }:

with lib;
(let cfg = config.modules.shell.tools.sshd;
in {
  options.modules.shell.tools.sshd.enable = mkOption {
    type = types.bool;
    default = false;
  };

  options.modules.shell.tools.sshd.enableFail2ban = mkOption {
    type = types.bool;
    default = true;
  };

  options.modules.shell.tools.sshd.enableGoogleAuthenticator = mkOption {
    type = types.bool;
    default = true;
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
