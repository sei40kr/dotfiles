{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.python;
  package = pkgs.python3.withPackages (p: with p; [ ipython ]);
in {
  options.modules.dev.python = {
    enable = mkBoolOpt false;
    poetry.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      ([ package black nodePackages.pyright ]
        ++ optionals cfg.poetry.enable [ poetry ]);
    env.PATH = [ "\${HOME}/.poetry/bin" ];

    modules.shell.zsh.rcInit = ''
      zinit ice wait''' lucid as'completion' id-as'OMZP::pip'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/pip/_pip
    '';
  };
}
