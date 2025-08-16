{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.hyprlang;
in
{
  options.modules.dev.lang.hyprlang = {
    enable = mkEnableOption "hyprlang";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ hyprls ];
  };
}
