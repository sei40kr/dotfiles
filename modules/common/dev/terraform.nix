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
      zsh.extraZinitCommands = ''
        zinit ice as'completion' wait'''
        zinit snippet OMZP::terraform/_terraform
      '';
    };
  };
}
