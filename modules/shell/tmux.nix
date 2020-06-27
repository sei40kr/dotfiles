{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.tmux;
  per-project-session =
    pkgs.callPackage <packages/tmux-plugins/per-project-session.nix> { };
in {
  options.modules.shell.tmux = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    autostart = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    modules.shell.zsh.tmuxInit = mkIf cfg.autostart ''
      if [[ -z "$TMUX" && -z "$INSIDE_EMACS" && -z "$EMACS" && -z "$VIM" ]]; then
        tmux
        exit
      fi
    '';

    my.home.programs.tmux = {
      baseIndex = 1;
      enable = true;
      extraConfig = "source-file ${escapeShellArg <config/tmux/extra.conf>}";
      sensibleOnTop = false;
      terminal = "tmux-256color";
      plugins = with pkgs.tmuxPlugins; [
        copycat
        open
        pain-control
        {
          plugin = sensible;
          extraConfig = "set-option -g prefix C-t";
        }
        {
          plugin = yank;
          extraConfig = "set-option -g @yank_with_mouse off";
        }
        per-project-session
      ];
    };

    my.packages = with pkgs; [
      ## Plugin Requirements
      # tmux-yank
      xsel
      # tmux-per-project-session
      fd
      fzf
    ];
  };
}
