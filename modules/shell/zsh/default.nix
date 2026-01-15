{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf optionalString types;
  inherit (types) lines;
  inherit (lib.my) mkBoolOpt mkOpt;

  shellCfg = config.modules.shell;
  cfg = shellCfg.zsh;
in
{
  options.modules.shell.zsh = {
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
      interactiveShellInit =
        let
          omz = path: "${pkgs.oh-my-zsh}/share/oh-my-zsh/${path}";
          omzl = path: omz "lib/${path}";
          omzp = path: omz "plugins/${path}";

          atuin-zhook =
            pkgs.runCommandLocal "atuin-zhook"
              {
                buildInputs = with pkgs; [
                  atuin
                  zsh
                ];
              }
              ''
                mkdir -p $out
                XDG_CONFIG_HOME=$(mktemp -d) XDG_DATA_HOME=$(mktemp -d) ${pkgs.atuin}/bin/atuin init zsh >$out/zhook.zsh
                zsh -c "zcompile $out/zhook.zsh"
              '';
          navi-zhook =
            pkgs.runCommandLocal "navi-zhook"
              {
                buildInputs = with pkgs; [
                  navi
                  zsh
                ];
              }
              ''
                mkdir -p $out
                ${pkgs.navi}/bin/navi widget zsh >$out/zhook.zsh
                zsh -c "zcompile $out/zhook.zsh"
              '';
          zoxide-zhook =
            pkgs.runCommandLocal "zoxide-zhook"
              {
                buildInputs = with pkgs; [
                  zoxide
                  zsh
                ];
              }
              ''
                mkdir -p $out
                ${pkgs.zoxide}/bin/zoxide init zsh >$out/zhook.zsh
                zsh -c "zcompile $out/zhook.zsh"
              '';
        in
        ''
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

          . "${zoxide-zhook}/zhook.zsh"

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
          zinit ice bindmap'^R->;\\ec->' multisrc'{completion,key-bindings}.zsh'
          zinit light '${pkgs.fzf}/share/fzf'
          bindkey -M emacs -r '^R'
          bindkey -M vicmd -r '^R'
          bindkey -M viins -r '^R'

          . "${atuin-zhook}/zhook.zsh"
          bindkey -M emacs -r '^[[A'
          bindkey -M emacs -r '^[[OA'

          zinit ice if'[[ "$TERM_PROGRAM" == "WezTerm" ]]'
          zinit light 'sei40kr/zsh-wez-man'

          zinit ice id-as'OMZP::bgnotify'
          zinit snippet ${omzp "bgnotify/bgnotify.plugin.zsh"}

          zinit ice id-as'OMZP::git-auto-fetch'
          zinit snippet ${omzp "git-auto-fetch/git-auto-fetch.plugin.zsh"}

          zinit ice id-as'OMZL::clipboard.zsh'
          zinit snippet ${omzl "clipboard.zsh"}

          HYPHEN_INSENSITIVE=true
          zinit ice id-as'OMZL::completion.zsh'
          zinit snippet ${omzl "completion.zsh"}
          bindkey -M menuselect '^[[Z' reverse-menu-complete

          . "${navi-zhook}/zhook.zsh"

          zinit ice id-as'OMZP::fancy-ctrl-z'
          zinit snippet ${omzp "fancy-ctrl-z/fancy-ctrl-z.plugin.zsh"}

          ${cfg.rcInit}
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
      navi
      zinit
      zoxide

      # For zinit
      curl
      file
      git
      subversion
    ];

    # Create an empty .zshrc to prevent zsh-newuser-install from running on start-up
    home.file.".zshrc".text = "";

    home.configFile."atuin/config.toml".source = ../../../config/atuin/config.toml;

    user.shell = pkgs.zsh;

    modules.shell.enable = true;
    modules.shell.starship.enable = true;
  };
}
