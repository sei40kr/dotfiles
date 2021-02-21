{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.ansible.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.ansible.enable {
    modules = {
      dev.editors.tools.packages = with pkgs;
        [
          # TODO Fix ansible-lint build & install it
          # ansible-lint
        ];
      shell.zsh.zinitPluginsInit = ''
        zinit snippet OMZP::ansible/ansible.plugin.zsh
      '';
    };

    user.packages = with pkgs; [ ansible ];
  };
}
