{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (pkgs.stdenv) isDarwin;
  shellCfg = config.modules.shell;
  cfg = shellCfg.zsh;
  dotDir = ".zsh";
in {
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;

    aliases = mkOpt attrs { };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        zsh

        # Plugin deps
        fzf
        my.zsh-fzf-chdir
        my.zsh-fzf-docker
        my.zsh-fzf-projects
        my.zsh-gh-clone
        my.zsh-ranger-cd

        # Completions
        nix-zsh-completions
        zsh-completions

        # Themes
        pure-prompt
      ] ++ optionals isDarwin [ terminal-notifier ];
    home.file = {
      "${dotDir}/completions".source = "${configDir}/zsh/completions";
      "${dotDir}/functions".source = "${configDir}/zsh/functions";
    };

    home-manager.users.${config.user.name}.programs = {
      zsh = {
        enable = true;
        enableCompletion = false;

        inherit dotDir;
        autocd = true;
        defaultKeymap = "emacs";
        shellAliases = shellCfg.aliases // cfg.aliases;
        localVariables = {
          KEYTIMEOUT = 1;

          FZF_PROJECTS_PROJECT_DIR_MAX_DEPTH = 2;

          PURE_PROMPT_SYMBOL = "λ";
        };

        envExtra = ''
          # Don't execute this file when running in a pure nix-shell.
          if [[ -n "$IN_NIX_SHELL" ]]; then
            return
          fi
        '' + (optionalString isDarwin ''
          if [[ -z "$__NIX_DARWIN_SET_ENVIRONMENT_DONE" ]]; then
            . ${config.system.build.setEnvironment}
          fi
        '');
        profileExtra = ''
          ${concatStringsSep "\n" (mapAttrsToList (n: v: ''export ${n}="${v}"'')
            (config.env // shellCfg.env))}
        '';
        initExtraFirst = ''
          # Disable some features to support TRAMP.
          if [[ "$TERM" == dumb ]]; then
            unsetopt ZLE PROMPT_CR PROMPT_SUBST
            unset RPS1 RPROMPT
            PS1='$ '
            PROMPT='$ '
            return
          fi

          ${optionalString shellCfg.tmux.autoRun ''
            if [[ -z "$TMUX" && -z "$EMACS" && -z "$VIM" && -z "$INSIDE_EMACS" && "$TERM_PROGRAM" != vscode ]]; then
              exec tmux new-session
            fi
          ''}
        '';
        initExtraBeforeCompInit = ''
          fpath+=( "''${ZDOTDIR}/functions" "''${ZDOTDIR}/completions" )

          GH_CLONE_WORKSPACE_DIR="$WORKSPACE_DIR"

          FZF_PROJECTS_WORKSPACE_DIRS=( "$WORKSPACE_DIR" );
          FZF_PROJECTS_KNOWN_PROJECTS=(
            "''${HOME}/.dotfiles"
            "''${HOME}/.emacs.d"
            "''${HOME}/.doom.d"
          )

          autoload -Uz gh-clone ranger-cd \
                       run-help run-help-git \
                       _fzf_complete_cd _fzf_complete_cd_post \
                       _fzf_complete_docker _fzf_complete_docker_post \
                       prompt_pure_setup \
                       vterm_printf
        '';
        initExtra = ''
          setopt APPEND_HISTORY AUTO_CD AUTO_LIST AUTO_MENU AUTO_PARAM_KEYS \
                 AUTO_PARAM_SLASH AUTO_PUSHD AUTO_RESUME EQUALS GLOB_DOTS \
                 HIST_REDUCE_BLANKS INTERACTIVE_COMMENTS NO_BEEP \
                 NUMERIC_GLOB_SORT PRINT_EIGHT_BIT PROMPT_SUBST \
                 PUSHD_IGNORE_DUPS
          unsetopt LIST_BEEP

          zle -N fzf-projects
          bindkey '^xg' fzf-projects
          bindkey '^x^g' fzf-projects

          zle -N ranger-cd
          bindkey '\ec' ranger-cd

          if [[ "$INSIDE_EMACS" == vterm ]]; then
            alias clear='vterm_printf "51; Evterm-clear-scrollback"; tput clear'
          fi

          # Prompt
          zstyle :prompt:pure:prompt:success color green
          zstyle :prompt:pure:git:dirty color 242
          prompt_pure_setup
        '';
      };

      direnv = {
        enable = true;
        enableZshIntegration = false;
        enableNixDirenvIntegration = true;
      };
    };

    environment.pathsToLink = [ "/share/zsh" ];

    modules.shell.zsh = {
      aliases = {
        d = "dirs";
        po = "popd";
        pu = "pushd";
      };

      zinit = {
        plugins = [
          # Plugins
          {
            source = "${pkgs.fzf}/share/fzf";
            ice = {
              bindmap = {
                "^R" = "";
                "\\ec" = "";
              };
              multisrc = "{completion,key-bindings}.zsh";
            };
            config = ''
              FZF_DEFAULT_OPTS='--height=15 --reverse --inline-info --color=dark --color=fg:-1,bg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef'
            '';
          }
          {
            source = "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions";
            ice = {
              wait = 0;
              lucid = true;
            };
          }
          {
            source =
              "${pkgs.zsh-you-should-use}/share/zsh/plugins/you-should-use";
            config = ''
              export YSU_MESSAGE_FORMAT="Found existing %alias_type for \"%command\": $(tput bold)%alias$(tput sgr0)"
              export YSU_HARDCORE=1
            '';
          }
          {
            source =
              "${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions";
            ice = {
              wait = 0;
              atinit = "zpcompinit; zpcdreplay";
              lucid = true;
            };
          }
          {
            source =
              "${pkgs.my.zsh-smart-command-history}/share/zsh/plugins/zsh-smart-command-history";
          }
          {
            source = "${pkgs.my.zsh-tmux-man}/share/zsh/plugins/zsh-tmux-man";
            config = ''
              if [[ "''${+aliases[run-help]}" == 1 ]]; then
                unalias run-help
              fi
            '';
          }
          {
            source = "hlissner/zsh-autopair";
            ice = {
              wait = 0;
              lucid = true;
            };
          }
          {
            source = "zdharma/history-search-multi-word";
            ice = {
              wait = 0;
              lucid = true;
            };
          }
          {
            source = "zdharma/null";
            ice = {
              atclone = "direnv hook zsh >zhook.zsh";
              atpull = "%atclone";
              id-as = "direnv";
            };
          }
        ];
        snippets = [
          # Plugins
          {
            source = "${pkgs.oh-my-zsh}/share/oh-my-zsh/lib/clipboard.zsh";
            ice.id-as = "OMZL::clipboard.zsh";
          }
          {
            source =
              "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/bgnotify/bgnotify.plugin.zsh";
            ice.id-as = "OMZP::bgnotify";
          }
          {
            source =
              "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh";
            ice.id-as = "OMZP::fancy-ctrl-z";
          }

          # Commands, Aliases
          {
            source =
              "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/extract/extract.plugin.zsh";
            ice = {
              trigger-load = [ "!extract" "!x" ];
              id-as = "OMZP::extract";
            };
          }
          {
            source =
              "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/nmap/nmap.plugin.zsh";
            ice.id-as = "OMZP::nmap";
          }
          {
            source =
              "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/rsync/rsync.plugin.zsh";
            ice.id-as = "OMZP::rsync";
          }
          {
            source =
              "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/zsh_reload/zsh_reload.plugin.zsh";
            ice.id-as = "OMZP::zsh_reload";
          }

          # Completions
          {
            source = "${pkgs.oh-my-zsh}/share/oh-my-zsh/lib/completion.zsh";
            ice.id-as = "OMZL::completion.zsh";
            config = ''
              HYPHEN_INSENSITIVE=true
            '';
          }
        ];
      };
    };
  };
}
