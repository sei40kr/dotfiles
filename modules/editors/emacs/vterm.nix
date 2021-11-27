{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs.vterm;

  vterm_printf = pkgs.writeTextFile {
    name = "vterm_printf";
    text = ''
      vterm_printf(){
          if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }
    '';
    destination = "/share/zsh/site-functions/vterm_printf";
  };
in {
  options.modules.editors.emacs.vterm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ vterm_printf ];

    modules.shell.zsh.rcInit = ''
      if [[ "$INSIDE_EMACS" == 'vterm' ]]; then
        autoload -Uz vterm_printf
        alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
      fi
    '';
  };
}
