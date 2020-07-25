{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.ansible.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.ansible.enable {
    my.packages = with pkgs; [ ansible ];
    modules.shell.zsh.zinitPluginsInit = ''
      zinit snippet OMZP::ansible/ansible.plugin.zsh
    '';
  };
}
