{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.terraform;
in {
  options.modules.dev.terraform = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ terraform terraform-ls ];

    modules.shell = {
      aliases.tf = "terraform";
      zsh.rcInit = ''
        zinit ice wait''' lucid as'completion' id-as'OMZP::terraform'
        zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/terraform/_terraform
      '';
    };
  };
}
