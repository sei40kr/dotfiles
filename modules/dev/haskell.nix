{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.haskell.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.haskell.enable {
    home = {
      packages = with pkgs; [ stack ];
      file.".stack/config.yml".source = <config/stack/config.yaml>;
    };
  };
}
