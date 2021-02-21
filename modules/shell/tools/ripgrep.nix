{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.shell.tools.ripgrep.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.ripgrep.enable {
    user.packages = with pkgs; [ ripgrep ];
    home.file.".ripgreprc".source = "${configDir}/ripgrep/ripgreprc";
  };
}
