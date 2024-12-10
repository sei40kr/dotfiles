{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.shell;
in
{
  options.modules.shell = with types; {
    enable = mkBoolOpt false;

    aliases = mkOpt (attrsOf (nullOr (either str path))) { };
    env = mkOpt attrs { };

    bat = {
      theme = mkOpt str null;
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      direnv
      du-dust
      fd
      eza
      htop
      prettyping
      procs
      rm-improved
      tealdeer
      xcp
    ];

    services.lorri.enable = true;

    modules.shell.aliases.u = "cd ..";

    # bat
    home-manager.users.${config.user.name}.programs.bat = {
      enable = true;
      config.theme = cfg.bat.theme;
    };
    modules.shell.aliases.cat = "bat";

    # dust
    modules.shell.aliases.du = "dust";

    # eza
    modules.shell.aliases.la = "eza -lbhHigUmuSa --time-style=long-iso --git --color-scale";
    modules.shell.aliases.ll = "eza -lbF --git";
    modules.shell.aliases.ls = "eza";
    modules.shell.aliases.tree = "eza -T";

    # htop
    modules.shell.aliases.top = "htop";

    # prettyping
    modules.shell.aliases.ping = "prettyping --nolegend";

    # procs
    modules.shell.aliases.ps = "procs";

    modules.shell.zsh.rcInit = ''
      export MANPAGER="sh -c 'col -bx | bat -l man -p'"
      export MANROFFOPT='-c'

      zinit ice src'zhook.zsh' id-as'direnv' atclone'direnv hook zsh >zhook.zsh' atpull'%atclone'
      zinit light zdharma-continuum/null
    '';
  };
}
