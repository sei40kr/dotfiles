{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell;
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
      nix-direnv
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
    modules.shell.aliases.la = "eza -laFh";
    modules.shell.aliases.ls = "eza -F";
    modules.shell.aliases.tree = "eza -T";

    # htop
    modules.shell.aliases.top = "htop";

    # prettyping
    modules.shell.aliases.ping = "prettyping --nolegend";

    # procs
    modules.shell.aliases.ps = "procs";

    modules.shell.zsh.rcInit = ''
      export MANPAGER="sh -c 'col -bx | bat -l man -p'"

      zinit ice src'zhook.zsh' id-as'direnv' atclone'direnv hook zsh >zhook.zsh' atpull'%atclone'
      zinit light zdharma-continuum/null

      zinit ice id-as'nix-direnv'
      zinit snippet ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
    '';
  };
}
