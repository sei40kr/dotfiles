{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.terraform.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.terraform.enable {
    modules = {
      dev.editors.tools.packages = with pkgs; [ terraform-ls ];
      shell.zsh.zinitPluginsInit = ''
        zinit ice as'completion' wait'''
        zinit snippet OMZP::terraform/_terraform
      '';
    };

    user.packages = with pkgs; [ terraform ];

    modules.shell.zsh.aliases.tf = "terraform";
  };
}
