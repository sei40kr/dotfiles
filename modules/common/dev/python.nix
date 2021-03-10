{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.python;
  package = pkgs.python3.withPackages (p:
    with p;
    [ ipython ]
    ++ optionals cfg.jupyter.enable [ jupyter matplotlib numpy pandas ]);
in {
  options.modules.dev.python = {
    enable = mkBoolOpt false;
    poetry.enable = mkBoolOpt false;
    jupyter.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      ([ package black python-language-server ]
        ++ optionals cfg.poetry.enable [ poetry ]);
    env.PATH = [ "\${HOME}/.poetry/bin" ];
    modules.shell.zsh.extraZinitCommands = ''
      zinit ice as'completion' wait'''
      zinit snippet OMZP::pip/_pip
    '' + optionalString cfg.poetry.enable ''
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
