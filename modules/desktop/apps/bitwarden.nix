{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.desktop.apps.bitwarden;
in
{
  options.modules.desktop.apps.bitwarden = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable { user.packages = with pkgs; [ bitwarden-desktop ]; };
}
