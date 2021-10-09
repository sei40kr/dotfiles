{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;

  zsh-vterm-printf = pkgs.writeTextFile {
    name = "/share/zsh/site-functions/vterm-printf";
    text = ''
      vterm-printf() {
        if [ -n $TMUX ]; then
          # tell tmux to pass the escape sequences through
          # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
          printf "\ePtmux;\e\e]%s\007\e\\" $1
        elif [ $TERM == screen-* ]; then
          # GNU screen (screen, screen-256color, screen-256color-bce)
          printf "\eP\e]%s\007\e\\" $1
        else
          printf "\e]%s\e\\" $1
        fi
      }
    '';
    destination = "/share/zsh/site-functions";
  };
  zsh-vterm-clear = pkgs.writeTextFile {
    name = "/share/zsh/site-functions/vterm-clear";
    text = ''
      clear() {
        vterm-printf "51;Evterm-clear-scrollback"
        tput clear
      }
    '';
    destination = "/share/zsh/site-functions";
  };
in {
  config = mkIf cfg.enable {
    user.packages = [ zsh-vterm-printf zsh-vterm-clear ];

    modules.shell.zsh.rcInit = ''
      if [[ "$INSIDE_EMACS" == vterm ]]; then
        autoload -Uz vterm-printf vterm-clear
        alias clear='vterm-clear'
      fi
    '';
  };
}
