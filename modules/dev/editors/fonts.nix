{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.editors.fonts.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.fonts.enable {
    my.packages = with pkgs;
      with pkgs.my; [
        fira-code
        jetbrains-mono
        operator-mono
        source-code-pro
      ];
  };
}
