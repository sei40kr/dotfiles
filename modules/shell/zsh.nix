{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.zsh;
  zdotDir = ".zsh";
  homeManagerInit = ''
    . ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  '';
  nixDarwinInit = ''
    if [[ -z "$__NIX_DARWIN_SET_ENVIRONMENT_DONE" ]]; then
      . ${config.system.build.setEnvironment}
    fi
  '';
  zinit = pkgs.my.zinit;
  aliasDefs = concatStringsSep "\n"
    (mapAttrsToList (k: v: "alias ${k}=${escapeShellArg v}") cfg.aliases);
in {
  options.modules.shell.zsh = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    tmuxInit = mkOption {
      visible = false;
      type = types.lines;
      default = "";
    };

    graphicalSessionInit = mkOption {
      visible = false;
      type = types.lines;
      default = "";
    };

    zinitPluginsInit = mkOption {
      type = types.lines;
      default = "";
    };

    aliases = mkOption {
      type = with types; attrsOf str;
      default = { };
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs; [ zsh ];
    my.home.home.file = {
      ".zshenv".text = ''
        ZDOTDIR="''${HOME}/${zdotDir}"
        . "''${ZDOTDIR}/.zshenv"
      '';
      "${zdotDir}/.zshenv".text = ''
        ${optionalString pkgs.stdenv.isDarwin nixDarwinInit}
        ${homeManagerInit}

        . ${<config/zsh/zshenv>}
      '';
      "${zdotDir}/.zprofile".text = ''
        . ${<config/zsh/zprofile>}

        ${cfg.graphicalSessionInit}
      '';
      "${zdotDir}/.zshrc".text = ''
        typeset -U path cdpath fpath manpath

        for profile in ''${(z)NIX_PROFILES}; do
          fpath+=(
            "''${profile}/share/zsh/site-functions"
            "''${profile}/share/zsh/''${ZSH_VERSION}/functions"
            "''${profile}/share/zsh/vendor-completions"
          )
        done
        HELPDIR="${pkgs.zsh}/share/zsh/''${ZSH_VERSION}/help"

        if [[ "$TERM" == dumb ]]; then
            HISTSIZE=0
            SAVEHIST=0
        else
          ${cfg.tmuxInit}

          . ${<config/zsh/before-zinit.zsh>}

          path=(
            ${pkgs.curl}/bin
            ${pkgs.fzf}/bin
            ${pkgs.git}/bin
            ${pkgs.subversion}/bin
            $path
          )
          declare -A ZINIT
          ZINIT[BIN_DIR]=${zinit}/share/zinit
          . "''${ZINIT[BIN_DIR]}/zinit.zsh"

          zinit light ${pkgs.my.zshPlugins.smart-command-history}/share/zsh/plugins/smart-command-history

          zinit ice trigger-load'__tmux_man' atinit'bindkey "^[h" __tmux_man'
          zinit light ${pkgs.my.zshPlugins.tmux-man}/share/zsh/plugins/tmux-man

          zinit ice has'fzf' trigger-load'!_fzf_complete_docker'
          zinit light ${pkgs.my.zshPlugins.fzf-docker}/share/zsh/plugins/fzf-docker

          zinit ice has'fzf' trigger-load'!_fzf_complete_cd'
          zinit light ${pkgs.my.zshPlugins.fzf-cd-dirs}/share/zsh/plugins/fzf-cd-dirs

          FZF_PROJECTS_WORKSPACE_DIRS=( "$PROJECT_DIR" )
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

          # TODO Move this in shell/tools/ranger module
          zinit ice trigger-load'!ranger-cd'
          zinit light ${pkgs.my.zshPlugins.ranger-cd}/share/zsh/plugins/ranger-cd
          bindkey '\ec' ranger-cd

          GH_CLONE_WORKSPACE_DIR="$PROJECT_DIR"
          zinit ice trigger-load'!gh-clone'
          zinit light ${pkgs.my.zshPlugins.gh-clone}/share/zsh/plugins/gh-clone

          ${cfg.zinitPluginsInit}

          ${aliasDefs}

          . ${<config/zsh/after-zinit.zsh>}
        fi
      '';
      "${zdotDir}/completions".source = <config/zsh/completions>;
      "${zdotDir}/functions".source = <config/zsh/functions>;
      "${zdotDir}/aliases.zsh".source = <config/zsh/aliases.zsh>;
    };
    my.home.xdg.configFile."starship.toml".source =
      <config/starship/starship.toml>;
  };
}
