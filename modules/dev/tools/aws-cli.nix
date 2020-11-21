{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.awsCli.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.awsCli.enable {
    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice wait''' lucid
      zinit snippet ${pkgs.awscli}/share/zsh/site-functions/aws_zsh_completer.sh
    '';

    my.packages = with pkgs; [ awscli ];
  };
}
