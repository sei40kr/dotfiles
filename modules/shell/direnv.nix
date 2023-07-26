{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.direnv;
in
{
  options.modules.shell.direnv = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ direnv nix-direnv ];

    modules.shell.zsh.rcInit = ''
      zinit ice src'zhook.zsh' id-as'direnv' atclone'direnv hook zsh >zhook.zsh' atpull'%atclone'
      zinit light zdharma-continuum/null

      zinit ice id-as'nix-direnv'
      zinit snippet ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
    '';
  };
}
