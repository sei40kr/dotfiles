{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (pkgs.stdenv) isDarwin;
  package = pkgs.zsh;
  shellCfg = config.modules.shell;
  cfg = shellCfg.zsh;
  dotDir = ".zsh";
in {
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;
    # TODO deprecate this
    extraZinitCommands = mkOpt lines "";
    aliases = mkOpt attrs { };
  };

  config = mkIf cfg.enable {
    user.packages = [ package ] ++ (with pkgs;
      [
        nix-zsh-completions

        # zinit deps
        curl
        fzf
        git
        subversion
      ] ++ optionals isDarwin [ terminal-notifier ]);
    home.file = {
      "${dotDir}/completions".source = "${configDir}/zsh/completions";
      "${dotDir}/functions".source = "${configDir}/zsh/functions";
    };

    home-manager.users.${config.user.name}.programs = {
      direnv = {
        enable = true;
        enableZshIntegration = false;
        enableNixDirenvIntegration = true;
      };
      zsh = {
        enable = true;
        enableCompletion = false;

        inherit dotDir;
        autocd = true;
        defaultKeymap = "emacs";
        shellAliases = shellCfg.aliases // cfg.aliases;
        localVariables.KEYTIMEOUT = 1;

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
          # TODO refactor this
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

          declare -A ZINIT
          ZINIT[BIN_DIR]=${pkgs.my.zinit}/share/zinit
          . "''${ZINIT[BIN_DIR]}/zinit.zsh"

          ${optionalString isDarwin ''
            zinit ice svn
            zinit snippet PZT::modules/gnu-utility
          ''}

          zinit ice atclone'direnv hook zsh >zhook.zsh' \
                    atpull'%atclone' \
                    id-as'direnv'
          zinit light zdharma/null

          zinit light ${pkgs.my.zshPlugins.smart-command-history}/share/zsh/plugins/smart-command-history


          zinit ice has'fzf' trigger-load'!_fzf_complete_docker'
          zinit light ${pkgs.my.zshPlugins.fzf-docker}/share/zsh/plugins/fzf-docker

          zinit ice has'fzf' trigger-load'!_fzf_complete_cd'
          zinit light ${pkgs.my.zshPlugins.fzf-cd-dirs}/share/zsh/plugins/fzf-cd-dirs

          FZF_PROJECTS_WORKSPACE_DIRS=( "$WORKSPACE_DIR" )
          FZF_PROJECTS_PROJECT_DIR_MAX_DEPTH=2
          FZF_PROJECTS_KNOWN_PROJECTS=(
              "''${HOME}/.dotfiles"
              "''${HOME}/.emacs.d"
              "''${HOME}/.doom.d"
          )
          zinit ice has'fzf' trigger-load'!fzf-projects'
          zinit light ${pkgs.my.zshPlugins.fzf-projects}/share/zsh/plugins/fzf-projects
          zle -N fzf-projects
          bindkey '^xg' fzf-projects
          bindkey '^x^g' fzf-projects

          # TODO Move this in shell/ranger module
          zinit ice trigger-load'!ranger-cd'
          zinit light ${pkgs.my.zshPlugins.ranger-cd}/share/zsh/plugins/ranger-cd
          bindkey '\ec' ranger-cd

          GH_CLONE_WORKSPACE_DIR="$WORKSPACE_DIR"
          zinit ice trigger-load'!gh-clone'
          zinit light ${pkgs.my.zshPlugins.gh-clone}/share/zsh/plugins/gh-clone

          . ${configDir}/zsh/after-zinit.zsh
        '';
        initExtra = ''
          setopt APPEND_HISTORY AUTO_CD AUTO_LIST AUTO_MENU AUTO_PARAM_KEYS \
                 AUTO_PARAM_SLASH AUTO_PUSHD AUTO_RESUME EQUALS GLOB_DOTS \
                 HIST_REDUCE_BLANKS INTERACTIVE_COMMENTS NO_BEEP \
                 NUMERIC_GLOB_SORT PRINT_EIGHT_BIT PROMPT_SUBST \
                 PUSHD_IGNORE_DUPS
          unsetopt LIST_BEEP
        '';
      };
    };

    environment.pathsToLink = [ "/share/zsh" ];

    modules.shell.zsh.aliases = {
      d = "dirs";
      po = "popd";
      pu = "pushd";
    };
  };
}
