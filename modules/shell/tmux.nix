{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.tmux;
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

    extraConfig = mkOption {
      type = types.lines;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    modules.shell.zsh.tmuxInit = mkIf cfg.autostart ''
      if [[ -z "$TMUX" && -z "$INSIDE_EMACS" && -z "$EMACS" && -z "$VIM" ]]; then
        ${pkgs.tmux}/bin/tmux
        exit
      fi
    '';

    my.home.programs.tmux = {
      baseIndex = 1;
      enable = true;
      extraConfig = ''
        source-file ${<config/tmux/tmux.conf>}

        ${cfg.extraConfig}
      '';
      sensibleOnTop = false;
      terminal = "tmux-256color";
      secureSocket = pkgs.stdenv.isLinux;
      plugins = with pkgs.tmuxPlugins;
        with pkgs.my.tmuxPlugins; [
          copycat
          open
          pain-control
          {
            plugin = sensible;
            extraConfig = "set-option -g prefix C-t";
          }
          {
            plugin = yank.overrideAttrs
              (oldAttrs: oldAttrs // { dependencies = with pkgs; [ xsel ]; });
            extraConfig = ''
              set-option -g @yank_with_mouse off
              set-option -g @custom_copy_command '${pkgs.xsel}/bin/xsel -i --clipboard'
            '';
          }
          {
            plugin = per-project-session;
            extraConfig = ''
              set-option -g detach-on-destroy off
              set-option -g @per-project-session-workspace-dirs "$WORKSPACE_DIR"
              set-option -g @per-project-session-known-project-dirs "''${HOME}/.dotfiles:''${HOME}/.emacs.d:''${HOME}/.doom.d"
              set-option -g @per-project-session-fzf-opts '-d 15 --reverse --inline-info --color=dark --color=fg:-1,bg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef'
            '';
          }
        ];
    };
  };
}
