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
      zi ice src'zhook.zsh' id-as'direnv' atclone'direnv hook zsh >zhook.zsh' atpull'%atclone'
      zi light z-shell/0

      zi ice id-as'nix-direnv'
      zi snippet ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
    '';
  };
}
