{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.nvim;

  package = pkgs.yonvim;
in
{
  options.modules.editors.nvim = with types; {
    enable = mkBoolOpt false;

    pager.enable = mkBoolOpt false;
    manpager.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [
      package
      (pkgs.yonvim-qt.override { yonvim = package; })
    ];

    environment.variables = {
      PAGER = mkIf cfg.pager.enable "${package}/bin/yonvim -c PAGER -";
      MANPAGER = mkIf cfg.manpager.enable "${package}/bin/yonvim +'Man!'";
      EDITOR = "${package}/bin/yonvim";
    };

    modules.shell.aliases = { vim = "nvim"; };
  };
}
