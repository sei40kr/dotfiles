{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.julia;
in
{
  options.modules.dev.lang.julia = {
    enable = mkEnableOption "Julia development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ julia-bin ];
  };
}
