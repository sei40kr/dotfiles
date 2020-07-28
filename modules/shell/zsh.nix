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
    my.packages = with pkgs; [ zsh git subversion ];

    my.home.home.file = {
      ".zshenv".text = ''
        ZDOTDIR="''${HOME}/${zdotDir}"
        . "''${ZDOTDIR}/.zshenv"
      '';
      "${zdotDir}/.zshenv".text = ''
        ${optionalString pkgs.stdenv.isDarwin nixDarwinInit}
        ${homeManagerInit}

        . ${escapeShellArg <config/zsh/env-extra.zsh>}
      '';
      "${zdotDir}/.zprofile".text = ''
        . ${escapeShellArg <config/zsh/profile-extra.zsh>}

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

        ${cfg.tmuxInit}

        HISTSIZE=10000
        SAVEHIST=10000
        HISTFILE="''${ZDOTDIR}/.zsh_history"

        setopt AUTO_CD
        setopt EXTENDED_HISTORY
        setopt HIST_IGNORE_DUPS
        setopt HIST_IGNORE_SPACE
        setopt SHARE_HISTORY

        bindkey -e

        HELPDIR="${pkgs.zsh}/share/zsh/''${ZSH_VERSION}/help"

        declare -A ZINIT
        ZINIT[BIN_DIR]=${escapeShellArg "${zinit}/share/zinit"}
        . "''${ZINIT[BIN_DIR]}/zinit.zsh"

        ${cfg.zinitPluginsInit}

        ${aliasDefs}

        . ${escapeShellArg <config/zsh/init-extra.zsh>}
      '';
      "${zdotDir}/completions".source = <config/zsh/completions>;
      "${zdotDir}/functions".source = <config/zsh/functions>;
      "${zdotDir}/aliases.zsh".source = <config/zsh/aliases.zsh>;
      "${zdotDir}/custom-history.zsh".source = <config/zsh/custom-history.zsh>;
    };
    my.home.xdg.configFile."starship.toml".source =
      <config/starship/starship.toml>;
  };
}
