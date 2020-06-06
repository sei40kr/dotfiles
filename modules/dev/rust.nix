{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.rust.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.rust.enable {
    my = {
      packages = with pkgs; [ rustup ];
      env = rec {
        CARGO_HOME = "\${HOME}/.cargo";
        PATH = [ "${CARGO_HOME}/bin" ];
      };
    };
  };
}
