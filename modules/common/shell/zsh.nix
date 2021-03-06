{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.zsh;
  zDotDir = ".zsh";
  zinit = pkgs.my.zinit;
in {
  options.modules.shell.zsh = with types; {
    enable = mkBoolOpt false;
    env = mkOpt attrs { };
    extraZinitCommands = mkOpt lines "";
    aliases = mkOpt attrs { };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      ([
        zsh
        nix-zsh-completions

        ## zinit dependencies
        curl
        fzf
        git
        subversion
      ] ++ optionals stdenv.isDarwin [ terminal-notifier ]);
    home.file = {
      "${zDotDir}/completions".source = "${configDir}/zsh/completions";
      "${zDotDir}/functions".source = "${configDir}/zsh/functions";
    };
    home-manager.users.${config.user.name}.programs = {
      direnv = {
        enable = true;
        enableNixDirenvIntegration = true;
        enableZshIntegration = false;
      };
      zsh = {
        enable = true;
        autocd = true;
        defaultKeymap = "emacs";
        dotDir = zDotDir;
        enableCompletion = false;
        sessionVariables = config.modules.shell.env // cfg.env;
        shellAliases = config.modules.shell.aliases // cfg.aliases;

        profileExtra = ''
          ${optionalString pkgs.stdenv.isDarwin ''
            eval "$(/usr/libexec/path_helper -s)"
          ''}

          fpath+=( "${zDotDir}/functions" "${zDotDir}/completions" )

          export PATH
          export FPATH
        '';
        initExtraFirst = ''
          ${optionalString config.modules.shell.tmux.autoRun.enable ''
            if [[ -z "$TMUX" && -z "$INSIDE_EMACS" && -z "$EMACS" && -z "$VIM" ]]; then
              tmux new-session && exit
            fi
          ''}

          if [[ -n "$TMUX" || -n "$INSIDE_EMACS" || -n "$EMACS" || -n "$VIM" ]]; then
            export PAGER=cat
          fi

          setopt APPEND_HISTORY
          setopt AUTO_CD
          setopt AUTO_LIST
          setopt AUTO_MENU
          setopt AUTO_PARAM_KEYS
          setopt AUTO_PARAM_SLASH
          setopt AUTO_PUSHD
          setopt AUTO_RESUME
          setopt EQUALS
          setopt GLOB_DOTS
          setopt HIST_REDUCE_BLANKS
          setopt INTERACTIVE_COMMENTS
          setopt NO_BEEP
          setopt NUMERIC_GLOB_SORT
          setopt PRINT_EIGHT_BIT
          setopt PROMPT_SUBST
          setopt PUSHD_IGNORE_DUPS
          unsetopt LIST_BEEP
        '';
        initExtraBeforeCompInit = with pkgs;
          with pkgs.my; ''
            declare -A ZINIT
            ZINIT[BIN_DIR]=${zinit}/share/zinit
            . "''${ZINIT[BIN_DIR]}/zinit.zsh"

            ${optionalString stdenv.isDarwin ''
              zinit ice svn
              zinit snippet PZT::modules/gnu-utility
            ''}

            zinit ice atclone'direnv hook zsh >zhook.zsh' \
                      atpull'%atclone' \
                      id-as'direnv'
            zinit light zdharma/null

            zinit light ${zshPlugins.smart-command-history}/share/zsh/plugins/smart-command-history

            zinit ice trigger-load'__tmux_man' atinit'bindkey "^[h" __tmux_man'
            zinit light ${zshPlugins.tmux-man}/share/zsh/plugins/tmux-man

            zinit ice has'fzf' trigger-load'!_fzf_complete_docker'
            zinit light ${zshPlugins.fzf-docker}/share/zsh/plugins/fzf-docker

            zinit ice has'fzf' trigger-load'!_fzf_complete_cd'
            zinit light ${zshPlugins.fzf-cd-dirs}/share/zsh/plugins/fzf-cd-dirs

            FZF_PROJECTS_WORKSPACE_DIRS=( "$PROJECT_DIR" )
            FZF_PROJECTS_PROJECT_DIR_MAX_DEPTH=2
            FZF_PROJECTS_KNOWN_PROJECTS=(
                "''${HOME}/.dotfiles"
                "''${HOME}/.emacs.d"
                "''${HOME}/.doom.d"
            )
            zinit ice has'fzf' trigger-load'!fzf-projects'
            zinit light ${zshPlugins.fzf-projects}/share/zsh/plugins/fzf-projects
            zle -N fzf-projects
            bindkey '^xg' fzf-projects
            bindkey '^x^g' fzf-projects

            # TODO Move this in shell/tools/ranger module
            zinit ice trigger-load'!ranger-cd'
            zinit light ${zshPlugins.ranger-cd}/share/zsh/plugins/ranger-cd
            bindkey '\ec' ranger-cd

            GH_CLONE_WORKSPACE_DIR="$PROJECT_DIR"
            zinit ice trigger-load'!gh-clone'
            zinit light ${zshPlugins.gh-clone}/share/zsh/plugins/gh-clone

            ${cfg.extraZinitCommands}

            . ${configDir}/zsh/after-zinit.zsh
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
