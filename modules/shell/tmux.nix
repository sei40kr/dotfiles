{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir;
  cfg = config.modules.shell.tmux;

  tmux-wrapped = pkgs.symlinkJoin {
    name = "tmux-wrapped";

    paths = with pkgs; [ tmux ];
    nativeBuildInputs = with pkgs; [ makeWrapper ];

    postBuild = ''
      wrapProgram $out/bin/tmux \
        --set __ETC_BASHRC_SOURCED "" \
        --set __ETC_ZSHENV_SOURCED "" \
        --set __ETC_ZPROFILE_SOURCED  "" \
        --set __ETC_ZSHRC_SOURCED "" \
        --set __NIX_SET_ENVIRONMENT_DONE "" \
        --set __NIX_DARWIN_SET_ENVIRONMENT_DONE ""
    '';
  };

in {
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;
    autoRun = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.tmux = {
      enable = true;

      package = tmux-wrapped;

      baseIndex = 1;
      escapeTime = 10;
      shortcut = "t";
      # NOTE macOS has ncurses version 5.7 which does not ship the terminfo
      #      description for tmux.
      #      https://gist.github.com/bbqtd/a4ac060d6f6b9ea6fe3aabe735aa9d95#the-fast-blazing-solution
      terminal =
        if pkgs.stdenv.isDarwin then "screen-256color" else "tmux-256color";

      plugins = with pkgs; [
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
        my.tmuxPlugins.ghq
      ];

      extraConfig = let inherit (config.modules.term.theme) colors;
      in ''
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

        set-option -g status-justify absolute-centre
        set-option -g status-style "bg=#${colors.tab.bg}"
        set-option -wg pane-active-border-style "fg=#${colors.border.active}"
        set-option -wg pane-border-style "fg=#${colors.border.inactive}"
        set-option -wg window-status-current-format " [#I] #W "
        set-option -wg window-status-current-style "fg=#${colors.tab.active.fg},bg=#${colors.tab.active.bg}"
        set-option -wg window-status-format " [#I] #W "
        set-option -wg window-status-separator " "
        set-option -wg window-status-style "fg=#${colors.tab.inactive.fg},bg=#${colors.tab.inactive.bg}"

        bind-key C new-session
        bind-key -T copy-mode-vi v send -X begin-selection
      '';

      tmuxinator.enable = true;
    };

    modules.shell.aliases.mux = "tmuxinator";
  };
}
