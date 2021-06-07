{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (pkgs.stdenv) isDarwin isLinux;
  cfg = config.modules.shell.tmux;
  prefix = "C-t";
in {
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;
    autoRun = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.tmux = {
      enable = true;

      inherit prefix;
      baseIndex = 1;
      escapeTime = 10;
      sensibleOnTop = false;
      secureSocket = isLinux;
      # The screen-256color in most cases is enough. But it does not support any
      # italic font style.
      # https://gist.github.com/bbqtd/a4ac060d6f6b9ea6fe3aabe735aa9d95#the-fast-blazing-solution
      terminal = if isDarwin then "screen-256color" else "tmux-256color";

      plugins = with pkgs; [
        my.tmuxPlugins.cleanup-unnamed-sessions
        tmuxPlugins.copycat
        tmuxPlugins.open
        tmuxPlugins.pain-control
        {
          plugin = tmuxPlugins.sensible;
          extraConfig = "set-option -g prefix ${prefix}";
        }
        {
          plugin = tmuxPlugins.yank;
          extraConfig = "set-option -g @yank_with_mouse off";
        }
        {
          plugin = my.tmuxPlugins.per-project-session;
          extraConfig = ''
            set-option -g detach-on-destroy off
            set-option -g @per-project-session-workspace-dirs "$WORKSPACE_DIR"
            set-option -g @per-project-session-known-project-dirs "''${HOME}/.dotfiles:''${HOME}/.emacs.d:''${HOME}/.doom.d"
            set-option -g @per-project-session-fzf-tmux-opts '-d 15 --reverse --inline-info --color=dark --color=fg:-1,bg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef'
          '';
        }
        my.tmuxPlugins.doom-one-dark
      ];

      extraConfig = ''
        set-option -as terminal-overrides ,alacritty:RGB
        set-option -g bell-action none
        set-option -g destroy-unattached off
        set-option -g focus-events off
        set-option -g renumber-windows on
        set-option -g set-titles on
        set-option -g wrap-search on
        set-option -g xterm-keys on
        set-option -wg mode-keys vi
        set-option -wg status-keys emacs

        bind-key C new-session
        bind-key -T copy-mode-vi v send -X begin-selection
      '';
    };
  };
}
