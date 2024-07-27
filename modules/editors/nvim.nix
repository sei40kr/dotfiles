{ config, lib, inputs', pkgs, ... }:

with lib;
with lib.my;
let
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.nvim;

  # NOTE: Yonvim requires Neovim 0.10.x
  yonvim = pkgs.yonvim.override { inherit (pkgs.unstable) neovim-unwrapped; };
  yonvim-qt = pkgs.yonvim-qt.override { inherit yonvim; };
in
{
  options.modules.editors.nvim = with types; {
    enable = mkBoolOpt false;

    pager.enable = mkBoolOpt false;
    manpager.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ yonvim yonvim-qt ];

    environment.variables = {
      PAGER = mkIf cfg.pager.enable "${yonvim}/bin/yonvim -c PAGER -";
      MANPAGER = mkIf cfg.manpager.enable "${yonvim}/bin/yonvim +'Man!'";
      EDITOR = "${yonvim}/bin/yonvim";
    };

    modules.shell.aliases = { vim = "nvim"; };
  };
}
