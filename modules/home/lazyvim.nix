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
  imports = [ inputs.self.homeModules.editor-shared ];

  options.modules.editors.lazyvim = {
    enable = mkEnableOption "LazyVim";
  };

  config = mkIf cfg.enable {
    home.packages = [
      (perSystem.lazyvim.default.override {
        inherit (pkgs) neovim-unwrapped wrapNeovimUnstable;
      })
      # For Snacks dashboard
      pkgs.dwt1-shell-color-scripts
      pkgs.gh
      pkgs.gh-notify
    ];
  };
}
