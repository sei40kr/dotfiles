{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.bitwarden;
in
{
  options.modules.desktop.apps.bitwarden = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable { user.packages = with pkgs; [ bitwarden ]; };
}
