{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.modules.editors.helix;
in
{
  options.modules.editors.helix = {
    enable = mkEnableOption "Helix";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ evil-helix ];
  };
}
