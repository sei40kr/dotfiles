{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (pkgs.stdenv.hostPlatform) system;
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
      inputs.yonvim.packages.${system}.yonvim

      # VTE terminals (ex. GNOME Terminal) does not support "Ms" capability.
      # See https://github.com/tmux/tmux/wiki/Clipboard#terminal-support---vte-terminals
      wl-clipboard
    ];

    env = {
      PAGER = mkIf cfg.pager.enable "${pkgs.neovim}/bin/nvim -c PAGER -";
      MANPAGER = mkIf cfg.manpager.enable "${pkgs.neovim}/bin/nvim +'Man!'";
    };

    modules.shell.aliases = { vim = "nvim"; };
  };
}
