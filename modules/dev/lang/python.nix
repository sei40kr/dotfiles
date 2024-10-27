{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.python;
in
{
  options.modules.dev.lang.python = {
    enable = mkBoolOpt false;

    enablePoetry = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages =
      with pkgs;
      [
        python3
        unstable.basedpyright
        ruff-lsp
      ]
      ++ optionals cfg.enablePoetry (with pkgs; [ poetry ]);
    env.PATH = [ "\${HOME}/.poetry/bin" ];

    modules.shell.zsh.rcInit = ''
      zinit ice wait''' lucid as'completion' id-as'OMZP::pip'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/pip/_pip
    '';
  };
}
