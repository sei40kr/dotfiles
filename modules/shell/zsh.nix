{ config, lib, options, pkgs, ... }:

with lib; {
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

  config = mkIf config.modules.shell.zsh.enable (let
    cfg = config.modules.shell.zsh;
    dotDir = ".zsh";
    zinit = builtins.fetchGit { url = "https://github.com/zdharma/zinit.git"; };
  in {
    my.home.programs.zsh = {
      enable = true;
      autocd = true;
      dotDir = dotDir;
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
        ZINIT[BIN_DIR]=${escapeShellArg zinit.outPath}

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
      "${dotDir}/completions".source = <config/zsh/completions>;
      "${dotDir}/functions".source = <config/zsh/functions>;
      "${dotDir}/aliases.zsh".source = <config/zsh/aliases.zsh>;
      "${dotDir}/custom-history.zsh".source = <config/zsh/custom-history.zsh>;
      "${dotDir}/secrets.zsh".source = <config/zsh/secrets.zsh>;
    };
    my.home.xdg.configFile."starship.toml".source =
      <config/starship/starship.toml>;
  });
}
