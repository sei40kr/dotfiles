{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.tools.gitu;
in
{
  options.modules.dev.tools.gitu = {
    enable = mkEnableOption "Gitu";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ gitu ];
  };
}
