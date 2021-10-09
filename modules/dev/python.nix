{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.python;
in {
  options.modules.dev.python = {
    enable = mkBoolOpt false;
    enablePoetry = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ python3 black nodePackages.pyright ]
      ++ optionals cfg.enablePoetry (with pkgs; [ poetry ]);
    env.PATH = [ "\${HOME}/.poetry/bin" ];

    modules.shell.zsh.rcInit = ''
      zinit ice wait''' lucid as'completion' id-as'OMZP::pip'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/pip/_pip
    '';
  };
}
