{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.editors.fonts.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.fonts.enable {
    my.packages = with pkgs; [ fira-code source-code-pro jetbrains-mono ];
  };
}
