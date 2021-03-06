{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.ansible;
in {
  options.modules.dev.ansible = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        ansible
        #ansible-lint
      ];
    modules.shell.zsh.extraZinitCommands = ''
      zinit snippet OMZP::ansible/ansible.plugin.zsh
    '';
  };
}
