{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.ansible;
in {
  options.modules.dev.ansible.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ ansible ansible-lint ];
    modules.shell.zsh.zinitPluginsInit = ''
      zinit snippet OMZP::ansible/ansible.plugin.zsh
    '';
  };
}
