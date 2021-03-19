{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.tmux;
in {
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;
    autoRun.enable = mkBoolOpt false;
    secureSocket.enable = mkBoolOpt pkgs.stdenv.isLinux;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.tmux = {
      baseIndex = 1;
      enable = true;
      extraConfig = ''
        set-option -as terminal-overrides ,*-256color*:Tc
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
      sensibleOnTop = false;
      terminal = "tmux-256color";
      secureSocket = cfg.secureSocket.enable;
      plugins = with pkgs; [
        my.tmuxPlugins.cleanup-unnamed-sessions
        tmuxPlugins.copycat
        tmuxPlugins.open
        tmuxPlugins.pain-control
        {
          plugin = tmuxPlugins.sensible;
          extraConfig = "set-option -g prefix C-t";
        }
        {
          plugin = tmuxPlugins.yank;
          # TODO Add supports for wl-clipboard
          extraConfig = ''
            set-option -g @yank_with_mouse off
            set-option -g @custom_copy_command '${pkgs.xsel}/bin/xsel -i --clipboard'
          '';
        }
        {
          plugin = my.tmuxPlugins.per-project-session;
          extraConfig = ''
            set-option -g detach-on-destroy off
            set-option -g @per-project-session-workspace-dirs "$PROJECT_DIR"
            set-option -g @per-project-session-known-project-dirs "''${HOME}/.dotfiles:''${HOME}/.emacs.d:''${HOME}/.doom.d"
            set-option -g @per-project-session-fzf-tmux-opts '-d 15 --reverse --inline-info --color=dark --color=fg:-1,bg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef'
          '';
        }
        my.tmuxPlugins.doom-one-dark
      ];
    };
  };
}
