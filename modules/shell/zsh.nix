{ config, lib, options, pkgs, ... }:

with lib;
let
  cfg = config.modules.shell.zsh;
  zdotDir = ".zsh";
  zinit = builtins.fetchGit { url = "https://github.com/zdharma/zinit.git"; };
in {
  options.modules.shell.zsh = {
    enable = mkOption {
      type = types.bool;
      default = false;
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
  };

  config = mkIf cfg.enable {
    my.home.programs.zsh = {
      enable = true;
      autocd = true;
      dotDir = zdotDir;
      enableCompletion = false;
      history = {
        size = 10000;
        save = 10000;
        ignoreDups = true;
        ignoreSpace = true;
        extended = true;
        share = true;
      };
      defaultKeymap = "emacs";
      initExtra = ''
        declare -A ZINIT
        ZINIT[BIN_DIR]=${escapeShellArg "${zinit}"}

        . "''${ZINIT[BIN_DIR]}/zinit.zsh"

        ${cfg.zinitPluginsInit}

        . ${escapeShellArg <config/zsh/init-extra.zsh>}
      '';
      envExtra = ''
        . ${escapeShellArg <config/zsh/env-extra.zsh>}
      '';
      profileExtra = ''
        . ${escapeShellArg <config/zsh/profile-extra.zsh>}

        ${cfg.graphicalSessionInit}
      '';
    };
    my.packages = with pkgs; [ subversion ]; # required by zinit

    my.home.home.file = {
      "${zdotDir}/completions".source = <config/zsh/completions>;
      "${zdotDir}/functions".source = <config/zsh/functions>;
      "${zdotDir}/aliases.zsh".source = <config/zsh/aliases.zsh>;
      "${zdotDir}/custom-history.zsh".source = <config/zsh/custom-history.zsh>;
      "${zdotDir}/secrets.zsh".source = <config/zsh/secrets.zsh>;
    };
    my.home.xdg.configFile."starship.toml".source =
      <config/starship/starship.toml>;
  };
}
