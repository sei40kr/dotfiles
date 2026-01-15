{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.editors.helix;
in
{
  options.modules.editors.helix = {
    enable = mkEnableOption "Helix";
  };

  config = mkIf cfg.enable {
    programs.helix = {
      enable = true;
      package = pkgs.evil-helix;
    };
  };
}
