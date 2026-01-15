{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.bitwarden;
in
{
  options.modules.desktop.apps.bitwarden = {
    enable = mkEnableOption "Bitwarden";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.bitwarden-desktop ];
  };
}
