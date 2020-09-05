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

        . ${escapeShellArg <config/zsh/zshenv>}
      '';
      "${zdotDir}/.zprofile".text = ''
        . ${escapeShellArg <config/zsh/zprofile>}

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

          . ${escapeShellArg <config/zsh/before-zinit.zsh>}

          path=(
            ${escapeShellArg "${pkgs.curl}/bin"}
            ${escapeShellArg "${pkgs.git}/bin"}
            ${escapeShellArg "${pkgs.subversion}/bin"}
            $path
          )
          declare -A ZINIT
          ZINIT[BIN_DIR]=${escapeShellArg "${zinit}/share/zinit"}
          . "''${ZINIT[BIN_DIR]}/zinit.zsh"

          ${cfg.zinitPluginsInit}

          ${aliasDefs}

          . ${escapeShellArg <config/zsh/after-zinit.zsh>}
        fi
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
