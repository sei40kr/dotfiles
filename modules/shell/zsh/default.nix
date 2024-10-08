{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  shellCfg = config.modules.shell;
  cfg = shellCfg.zsh;

  starship-nerd-font-symbols-preset = pkgs.runCommandLocal "starship-nerd-font-symbols-preset-${pkgs.starship.version}" { } ''
    mkdir -p $out/etc
    ${pkgs.starship}/bin/starship preset nerd-font-symbols >$out/etc/starship.toml
  '';
in
{
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;

    envInit = mkOpt lines "";
    rcInit = mkOpt lines "";
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      shellAliases = shellCfg.aliases;
      shellInit = ''
        ${cfg.envInit}
      '';
      interactiveShellInit = let 
        omz = path: "${pkgs.oh-my-zsh}/share/oh-my-zsh/${path}";
        omzl = path: omz "lib/${path}";
        omzp = path: omz "plugins/${path}";
      in ''
        ${optionalString shellCfg.tmux.autoRun ''
          if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIMRUNTIME" && -z "$INSIDE_EMACS" && "$TERM_PROGRAM" != 'WezTerm' ]]; then
            tmux start-server

            if ! tmux has-session 2>/dev/null; then
              tmux new-session -ds main \; set-option -t main destroy-unattached off
            fi

            exec tmux attach-session -d
          fi
        ''}

        KEYTIMEOUT=1
        bindkey -e
        bindkey '^u' backward-kill-line
        autoload -Uz select-word-style
        select-word-style bash

        declare -A ZINIT
        ZINIT[BIN_DIR]=${pkgs.zinit}/share/zinit
        . "''${ZINIT[BIN_DIR]}/zinit.zsh"
        # This conflicts with zoxide
        unalias zi

        zinit ice trigger-load'!extract;!x' id-as'OMZP::extract'
        zinit snippet ${omzp "extract/extract.plugin.zsh"}
        
        zinit ice ver'66f6ba7549b9973eb57bfbc188e29d2f73bf31bb' trigger-load'!cd-gitroot'
        zinit light 'mollifier/cd-gitroot'
        alias U='cd-gitroot'

        eval "$(zoxide init zsh)"
        
        ZSH_AUTOSUGGEST_STRATEGY=(atuin)
        _zsh_autosuggest_strategy_atuin() {
          typeset -g suggestion
          suggestion="$(atuin search --limit 1 --search-mode prefix --cmd-only "$1")"
        }
        zinit ice wait'0' lucid
        zinit light ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions
        
        zinit ice wait'0' lucid
        zinit light ${pkgs.zsh-autopair}/share/zsh/zsh-autopair

        zinit ice ver'v1.55' wait lucid atinit'ZINIT[COMPINIT_OPTS]=-C; zpcompinit; zpcdreplay'
        zinit light z-shell/F-Sy-H
        
        FZF_DEFAULT_OPTS='--cycle --reverse --info=inline'
        FZF_DEFAULT_COMMAND='fd -HL -t f -E .git'
        FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
        FZF_CTRL_T_OPTS="--preview='bat -pp --color=always {}'"
        FZF_ALT_C_OPTS="--preview='eza -la {}'"
        FZF_TMUX=1
        FZF_TMUX_OPTS='-p 80%,80%'
        zinit ice bindmap'^R->;\\ec->' multisrc'{completion,key-bindings}.zsh'
        zinit light '${pkgs.fzf}/share/fzf'
        bindkey -M emacs -r '^R'
        bindkey -M vicmd -r '^R'
        bindkey -M viins -r '^R'
        
        zinit ice src'zhook.zsh' id-as'atuin' atclone'atuin init zsh >zhook.zsh' atpull'%atclone'
        zinit light zdharma-continuum/null
        bindkey -M emacs -r '^[[A'
        bindkey -M emacs -r '^[[OA'
        
        if [[ -n "$TMUX" ]]; then
          zinit ice ver'37a3c697461b33dd2f85998431cdfec6d963be37'
          zinit light 'sei40kr/zsh-tmux-man'
          # Remove alias run-help=man
          if [[ ''${+aliases[run-help]} == 1 ]]; then
            unalias run-help
          fi
        fi

        zinit ice id-as'OMZP::bgnotify'
        zinit snippet ${omzp "bgnotify/bgnotify.plugin.zsh"}

        zinit ice id-as'OMZP::git-auto-fetch'
        zinit snippet ${omzp "git-auto-fetch/git-auto-fetch.plugin.zsh"}

        zinit ice id-as'OMZL::clipboard.zsh'
        zinit snippet ${omzl "clipboard.zsh"}

        HYPHEN_INSENSITIVE=true
        zinit ice id-as'OMZL::completion.zsh'
        zinit snippet ${omzl "completion.zsh"}

        zinit ice from'gh-r' ver'v2.19.0' as'program' atclone'./navi widget zsh >zhook.zsh' atpull'%atclone' src'zhook.zsh'
        zinit light denisidoro/navi

        zinit ice id-as'OMZP::fancy-ctrl-z'
        zinit snippet ${omzp "fancy-ctrl-z/fancy-ctrl-z.plugin.zsh"}

        ${cfg.rcInit}
      '';
      promptInit = ''
        eval "$(starship init zsh)"
      '';
      histSize = 10000;
      histFile = "$HOME/.zsh/.zsh_history";
      setOptions = [
         # Pushes the old directory onto the directory stack on cd
         "AUTO_PUSHD"
         # Doesn't store duplicates in the directory stack
         "PUSHD_IGNORE_DUPS"
         # Automatically lists possibilities on an ambiguous completion
         "AUTO_LIST"
         # Automatically uses menu completion after the second consecutive request for completion
         "AUTO_MENU"
         # Treats the keys of associative arrays as potential matches for parameter name completion
         "AUTO_PARAM_KEYS"
         # Appends a slash to completed parameters that are directories
         "AUTO_PARAM_SLASH"
         # Treats the character '=' as a word separator
         "EQUALS"
         # Includes hidden files in globbing results
         "GLOB_DOTS"
         # Sorts filenames numerically when they appear in a globbing pattern
         "NUMERIC_GLOB_SORT"
         # Appends history lines to the history file as they are entered
         "APPEND_HISTORY"
         # Uses an fcntl lock while reading and writing the history file
         "HIST_FCNTL_LOCK"
         # Doesn't enter command lines into the history list if they are duplicates of any previous event
         "HIST_IGNORE_ALL_DUPS"
         # Doesn't enter command lines into the history list if they are duplicates of the previous event
         "HIST_IGNORE_DUPS"
         # Doesn't save lines that start with a space in the history list
         "HIST_IGNORE_SPACE"
         # Removes superfluous blanks from each command line being added to the history list
         "HIST_REDUCE_BLANKS"
         # Shares history between all sessions
         "SHARE_HISTORY"
         # Allows comments in interactive shells
         "INTERACTIVE_COMMENTS"
         # Prints eight-bit characters literally
         "PRINT_EIGHT_BIT"
         # Tries to resume suspended jobs before starting a new process
         "AUTO_RESUME"
         # Performs parameter expansion, command substitution, and arithmetic expansion on the prompt
         "PROMPT_SUBST"
         # Suppresses the beep on error
         "NO_BEEP"
      ];
      enableCompletion = true;
      enableBashCompletion = false;
    };

    environment.systemPackages = with pkgs; [
      atuin
      fd
      fzf
      starship
      zoxide

      # For zinit
      curl
      file
      git
      subversion
    ];

    home.configFile."atuin/config.toml".source = "${configDir}/atuin/config.toml";

    home.configFile."starship.toml".text = ''
      ${builtins.readFile "${starship-nerd-font-symbols-preset}/etc/starship.toml"}
    '';

    user.shell = pkgs.zsh;

    modules.shell.enable = true;
  };
}
