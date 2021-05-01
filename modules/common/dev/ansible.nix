{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.ansible;
  package = pkgs.ansible.overrideAttrs (_: { doCheck = false; });
in {
  options.modules.dev.ansible = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = [ package ];
    modules.shell.zsh.extraZinitCommands = ''
      zinit snippet OMZP::ansible/ansible.plugin.zsh
    '';
  };
}
