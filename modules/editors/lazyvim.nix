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
      (inputs'.lazyvim.packages.default.override { inherit (pkgs.unstable) neovim-unwrapped; })
    ];
  };
}
