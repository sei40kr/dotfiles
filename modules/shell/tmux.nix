{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.tmux.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tmux.enable {
    programs.tmux = {
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
