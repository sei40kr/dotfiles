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

    programs.git.ignores = [
      # Swap
      "[._]*.s[a-v][a-z]"
      "!*.svg"
      "[._]*.sw[a-p]"
      "[._]s[a-rt-v][a-z]"
      "[._]ss[a-gi-z]"
      "[._]sw[a-p]"
      # Session
      "Session.vim"
      "Sessionx.vim"
      # Temporary
      ".netrwhist"
      "*~"
      # Auto-generated tag files
      "tags"
      # Persistent undo
      "[._]*.un~"
    ];
  };
}
