{
  config,
  inputs,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.editors.lazyvim;
in
{
  options.modules.editors.lazyvim = {
    enable = mkEnableOption "LazyVim";
  };

  config = mkIf cfg.enable {
    home.packages = [
      perSystem.lazyvim.default
      # For Snacks dashboard
      pkgs.dwt1-shell-color-scripts
      pkgs.gh
      pkgs.gh-notify
    ];
  };
}
