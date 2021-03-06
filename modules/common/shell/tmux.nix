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
        source-file ${configDir}/tmux/tmux.conf
      '';
      sensibleOnTop = false;
      terminal = "tmux-256color";
      secureSocket = cfg.secureSocket.enable;
      plugins = with pkgs.tmuxPlugins;
        with pkgs.my.tmuxPlugins; [
          cleanup-unnamed-sessions
          copycat
          open
          pain-control
          {
            plugin = sensible;
            extraConfig = "set-option -g prefix C-t";
          }
          {
            plugin = yank;
            # TODO Add supports for wl-clipboard
            extraConfig = ''
              set-option -g @yank_with_mouse off
              set-option -g @custom_copy_command '${pkgs.xsel}/bin/xsel -i --clipboard'
            '';
          }
          {
            plugin = per-project-session;
            extraConfig = ''
              set-option -g detach-on-destroy off
              set-option -g @per-project-session-workspace-dirs "$PROJECT_DIR"
              set-option -g @per-project-session-known-project-dirs "''${HOME}/.dotfiles:''${HOME}/.emacs.d:''${HOME}/.doom.d"
              set-option -g @per-project-session-fzf-tmux-opts '-d 15 --reverse --inline-info --color=dark --color=fg:-1,bg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef'
            '';
          }
          doom-one-dark
        ];
    };
  };
}
