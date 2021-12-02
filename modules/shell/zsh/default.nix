{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  inherit (pkgs.stdenv) isDarwin;
  shellCfg = config.modules.shell;
  cfg = shellCfg.zsh;
in {
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;

    envInit = mkOpt lines "";
    rcInit = mkOpt lines "";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      zsh

      fzf
      pure-prompt
    ];

    environment.etc.zshenv.text = let
      system_zsh-completions = pkgs.buildEnv {
        name = "system_zsh-completions";
        paths = config.environment.systemPackages;
        pathsToLink = [ "/share/zsh" ];
        ignoreCollisions = true;
      };
    in ''
      # Only execute this file once per shell.
      if [[ -n "$__ETC_ZSHENV_SOURCED" ]]; then
        return
      fi
      export __ETC_ZSHENV_SOURCED=1

      if [[ -z "$__NIXOS_SET_ENVIRONMENT_DONE" && -z "$__NIX_DARWIN_SET_ENVIRONMENT_DONE" ]]; then
        . ${config.system.build.setEnvironment}
      fi

      fpath=(
        "${system_zsh-completions}/share/zsh/site-functions"
        "${system_zsh-completions}/share/zsh/''${ZSH_VERSION}/functions"
        "${system_zsh-completions}/share/zsh/vendor-completions"
        "''${fpath[@]}"
      )

      # Read system-wide modifications.
      if [[ -f /etc/zshenv.local ]]; then
        . /etc/zshenv.local
      fi
    '';

    home-manager.users.${config.user.name}.programs.zsh = {
      enable = true;
      enableCompletion = false;
      completionInit = "";
      autocd = true;
      dotDir = ".config/zsh";
      defaultKeymap = "emacs";
      shellAliases = shellCfg.aliases;
      envExtra = let
        user_zsh-completions = pkgs.buildEnv {
          name = "user_zsh-completions";
          paths = config.users.users.${config.user.name}.packages;
          pathsToLink = [ "/share/zsh" ];
          ignoreCollisions = true;
        };
      in ''
        # Don't execute this file when running in a pure nix-shell
        if [[ -n "$IN_NIX_SHELL" ]]; then
          return
        fi

        fpath=(
          "${user_zsh-completions}/share/zsh/site-functions"
          "${user_zsh-completions}/share/zsh/''${ZSH_VERSION}/functions"
          "${user_zsh-completions}/share/zsh/vendor-completions"
          "${pkgs.zsh-completions}/share/zsh/site-functions"
          "''${fpath[@]}"
        )

        ${cfg.envInit}
      '';
      profileExtra = ''
        ${concatStringsSep "\n" (mapAttrsToList (n: v: ''export ${n}="${v}"'')
          (config.env // shellCfg.env))}
      '';
      initExtraFirst = ''
        ${optionalString config.modules.desktop.sway.enable ''
          if [[ -z $DISPLAY && "$(tty)" == /dev/tty1 ]]; then
            exec sway
          fi
        ''}
        ${optionalString shellCfg.tmux.autoRun ''
          if [[ -z $TMUX && -z $EMACS && -z $VIM && -z $INSIDE_EMACS ]]; then
            tmux start-server

            if ! tmux has-session 2>/dev/null; then
              tmux new-session -ds main \; \
                   set-option -t main destroy-unattached off
            fi

            exec tmux attach-session -d
          fi
        ''}


        # Disable some features to support TRAMP
        if [[ $TERM == dumb ]]; then
          unsetopt ZLE PROMPT_CR PROMPT_SUBST
          unset RPS1 RPROMPT
          PS1='$ '
          PROMPT='$ '
          return
        fi


        setopt APPEND_HISTORY AUTO_LIST AUTO_MENU AUTO_PARAM_KEYS \
               AUTO_PARAM_SLASH AUTO_PUSHD AUTO_RESUME EQUALS GLOB_DOTS \
               HIST_REDUCE_BLANKS INTERACTIVE_COMMENTS NO_BEEP \
               NUMERIC_GLOB_SORT PRINT_EIGHT_BIT PROMPT_SUBST \
               PUSHD_IGNORE_DUPS
        unsetopt LIST_BEEP
      '';
      initExtraBeforeCompInit = ''
        KEYTIMEOUT=1
        autoload -Uz select-word-style
        select-word-style bash
        bindkey '^u' backward-kill-line


        PATH="''${PATH:+$PATH:}${
          makeBinPath (with pkgs; [ curl git subversion ])
        }"
        declare -A ZINIT
        ZINIT[BIN_DIR]=${pkgs.zinit}/share/zinit
        . ''${ZINIT[BIN_DIR]}/zinit.zsh


        ## Commands

        zinit ice trigger-load '!extract;!x' id-as'OMZP::extract'
        zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/extract/extract.plugin.zsh

        ${optionalString isDarwin ''
          zinit ice id-as'PZT::modules--gnu-utility'
          zinit light ${pkgs.zsh-prezto}/modules/gnu-utility
        ''}

        ## Shell Enhancements

        zinit ice wait'0' lucid
        zinit light ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions

        zinit ice wait'0' lucid
        zinit light hlissner/zsh-autopair

        zinit ice wait'0' atinit'zpcompinit; zpcdreplay' lucid
        zinit light ${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions

        zinit ice wait'0' lucid
        zinit light zdharma/history-search-multi-word

        zinit light ${pkgs.my.zsh-smart-history}/share/zsh/plugins/zsh-smart-history

        zinit ice load'[[ -n $TMUX ]]' unload'[[ -z $TMUX ]]'
        zinit light ${pkgs.my.zsh-tmux-man}/share/zsh/plugins/zsh-tmux-man
        # Remove alias run-help=man
        if [[ ''${+aliases[run-help]} == 1 ]]; then
          unalias run-help
        fi

        ${optionalString isDarwin ''
          PATH="''${PATH:+$PATH:}${pkgs.terminal-notifier}/bin"
        ''}
        zinit ice id-as'OMZP::bgnotify'
        zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/bgnotify/bgnotify.plugin.zsh

        zinit ice id-as'OMZL::clipboard.zsh'
        zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/lib/clipboard.zsh

        HYPHEN_INSENSITIVE=true
        zinit ice id-as'OMZL::completion.zsh'
        zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/lib/completion.zsh

        zinit ice id-as'OMZP::zsh_reload'
        zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/zsh_reload/zsh_reload.plugin.zsh


        ## Key Bindings

        zinit ice id-as'OMZP::fancy-ctrl-z'
        zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh

        FZF_DEFAULT_OPTS='--height=15 --reverse --inline-info --color=dark --color=fg:-1,bg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef'
        zinit ice bindmap'^R->;\\ec->' multisrc'{completion,key-bindings}.zsh'
        zinit light ${pkgs.fzf}/share/fzf


        ## Prompt

        PURE_PROMPT_SYMBOL='λ'
        zstyle :prompt:pure:prompt:success color green
        zstyle :prompt:pure:git:dirty color 242
        autoload -Uz prompt_pure_setup
        prompt_pure_setup
      '';
      initExtra = ''
        ${cfg.rcInit}
      '';
    };

    environment.shells =
      [ "/run/current-system/sw/bin/zsh" "${pkgs.zsh}/bin/zsh" ];
    user.shell = pkgs.zsh;
  };
}
