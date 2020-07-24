{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.dev.python;
in {
  options.modules.dev.python = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enablePoetry = mkOption {
      type = types.bool;
      default = true;
    };
  };

  config = mkIf cfg.enable {
    my.packages = with pkgs;
      ([
        python37
        python37Packages.matplotlib
        python37Packages.numpy
        python37Packages.jupyter
        python37Packages.pandas
      ] ++ optionals cfg.enablePoetry [ poetry ]);
    my.env.PATH = [ "\${HOME}/.poetry/bin" ];
    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice as'completion' wait'''
      zinit snippet OMZP::pip/_pip
    '' + optionals cfg.enablePoetry ''
      zinit ice wait''' \
                lucid \
                atclone'poetry completions zsh >_poetry' \
                atpull'%atclone' \
                as'completion' \
                id-as'poetry_completion'
      zinit light zdharma/null
    '';
  };
}
