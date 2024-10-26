{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.lean;
in
{
  options.modules.dev.lang.lean = {
    enable = mkEnableOption "Lean language support";
  };

  config = mkIf cfg.enable { environment.systemPackages = with pkgs; [ lean4 ]; };
}
