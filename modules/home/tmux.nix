{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption;

  cfg = config.modules.term.tmux;

  tmux-project = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "project";
    version = inputs.tmux-project.shortRev;
    src = inputs.tmux-project;
  };
in
{
  options.modules.term.tmux = {
    enable = mkEnableOption "tmux";
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      clock24 = true;
      customPaneNavigationAndResize = true;
      disableConfirmationPrompt = true;
      focusEvents = true;
      escapeTime = 0;
      keyMode = "vi";
      newSession = true;
      sensibleOnTop = true;
      terminal = "tmux-256color";
      prefix = "C-t";
      plugins = [
        pkgs.tmuxPlugins.jump
        {
          plugin = pkgs.tmuxPlugins.minimal-tmux-status;
          extraConfig = ''
            set -g @minimal-tmux-use-arrow true
            set -g @minimal-tmux-right-arrow ""
            set -g @minimal-tmux-left-arrow ""
            set -g @minimal-tmux-window-status-format ' #{?window_bell_flag,#[fg=yellow]•#[default],}#I:#W '
          '';
        }
        pkgs.tmuxPlugins.pain-control
        {
          plugin = tmux-project;
          extraConfig = ''
            set -g @project-base-dirs "/etc/dotfiles,$HOME/ghq:3:3"
          '';
        }
        {
          plugin = pkgs.tmuxPlugins.sessionist;
          extraConfig = ''
            # Use a non-existent key to effectively unbind the default 'g' binding
            set -g @sessionist-goto "F20"
          '';
        }
        pkgs.tmuxPlugins.tmux-thumbs
        {
          plugin = pkgs.tmuxPlugins.vim-tmux-navigator;
          extraConfig = ''
            set -g @vim_navigator_mapping_left "M-h"
            set -g @vim_navigator_mapping_right "M-l"
            set -g @vim_navigator_mapping_up "M-k"
            set -g @vim_navigator_mapping_down "M-j"
            set -g @vim_navigator_mapping_prev "M-\\"
          '';
        }
      ];
      extraConfig = ''
        set -g detach-on-destroy no-detached
        set -g set-clipboard on
        set -as terminal-features ",*:hyperlinks:RGB:strikethrough"
        set -g monitor-bell on
        set -g pane-active-border-style 'bg=#1f2335,fg=#1b1d26'
        set -g pane-border-style 'bg=#1f2335,fg=#1b1d26'
        set -g window-status-bell-style default
        set -g window-active-style 'fg=#c0caf5,bg=#16161e'
        set -g window-style 'dim,bg=#1a1b26'
        set -g status-style 'fg=#565f89,bg=#1a1b26'
        set -g message-style 'fg=#7aa2f7,bg=#1a1b26'
        set -g mode-style 'fg=#7aa2f7,bg=#3b4261'

        # Vim-like copy mode bindings
        bind -T copy-mode-vi v send -X begin-selection
        bind -T copy-mode-vi V send -X select-line
        bind -T copy-mode-vi C-v send -X begin-selection \; send -X rectangle-toggle
        bind -T copy-mode-vi y send -X copy-selection-and-cancel
        bind -T copy-mode-vi Escape send -X cancel
      '';
    };
  };
}
