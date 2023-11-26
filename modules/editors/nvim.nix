{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.nvim;
in
{
  options.modules.editors.nvim = with types; {
    enable = mkBoolOpt false;

    pager.enable = mkBoolOpt false;
    manpager.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      yonvim
      yonvim-qt
    ];

    env = {
      PAGER = mkIf cfg.pager.enable "${pkgs.neovim}/bin/nvim -c PAGER -";
      MANPAGER = mkIf cfg.manpager.enable "${pkgs.neovim}/bin/nvim +'Man!'";
    };

    modules.shell.aliases = { vim = "nvim"; };
  };
}
