{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.expressvpn;
in
{
  options.modules.services.expressvpn = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ expressvpn ];

    services.expressvpn.enable = true;
  };
}
