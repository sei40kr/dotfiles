{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir;
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
        {
          plugin = tmuxPlugins.sensible;
          extraConfig = "set-option -g prefix ${prefix}";
        }
        tmuxPlugins.copycat
        tmuxPlugins.open
        tmuxPlugins.pain-control
        tmuxPlugins.urlview
        {
          plugin = tmuxPlugins.sessionist;
          extraConfig = ''
            set-option -g @sessionist-goto '''
          '';
        }
        {
          plugin = tmuxPlugins.yank;
          extraConfig = "set-option -g @yank_with_mouse off";
        }
        my.tmuxPlugins.cowboy
        my.tmuxPlugins.doom-one-dark
      ];

      extraConfig = ''
        set-option -as terminal-overrides ,alacritty:RGB
        set-option -g bell-action none
        set-option -g destroy-unattached on
        set-option -g focus-events off
        set-option -g renumber-windows on
        set-option -g set-titles on
        set-option -g wrap-search on
        set-option -g xterm-keys on
        set-option -wg mode-keys vi
        set-option -wg status-keys emacs

        bind-key C new-session
        bind-key g run-shell -b ${binDir}/tmux/create-or-switch-to-ghq-session
        bind-key -T copy-mode-vi v send -X begin-selection
      '';

      tmuxinator.enable = true;
    };

    modules.shell.aliases.mux = "tmuxinator";
  };
}
