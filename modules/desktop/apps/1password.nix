{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps._1password;
in
{
  options.modules.desktop.apps._1password = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs = {
      _1password.enable = true;
      _1password-gui = {
        enable = true;
        polkitPolicyOwners = [ config.user.name ];
      };
    };
  };
}
