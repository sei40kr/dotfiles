{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.zsh.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.zsh.enable (let
    dotDir = ".zsh";
    zinit = builtins.fetchGit { url = "https://github.com/zdharma/zinit.git"; };
  in {
    my.home.programs.zsh = {
      enable = true;
      autocd = true;
      dotDir = dotDir;
      history = {
        size = 10000;
        save = 10000;
        ignoreDups = true;
        ignoreSpace = true;
        extended = true;
        share = true;
      };
      defaultKeymap = "emacs";
      initExtraBeforeCompInit = ''
        declare -A ZINIT
        ZINIT[BIN_DIR]=${escapeShellArg zinit.outPath}

        . ${escapeShellArg <config/zsh/init-extra-before-compinit.zsh>}
      '';
      initExtra = ''
        . ${escapeShellArg <config/zsh/init-extra.zsh>}
      '';
      envExtra = ''
        . ${escapeShellArg <config/zsh/env-extra.zsh>}
      '';
      profileExtra = ''
        . "''${HOME}/.nix-profile/etc/profile.d/nix.sh"

        . ${escapeShellArg <config/zsh/profile-extra.zsh>}
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
