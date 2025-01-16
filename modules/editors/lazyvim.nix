{
  config,
  inputs',
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.modules.editors.lazyvim;
in
{
  options.modules.editors.lazyvim = {
    enable = mkEnableOption "LazyVim";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      inputs'.lazyvim.packages.default
      # For Snacks dashboard
      pkgs.dwt1-shell-color-scripts
      pkgs.gh
      pkgs.gh-notify
    ];
  };
}
