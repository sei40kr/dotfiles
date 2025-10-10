{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.services.expressvpn;
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
