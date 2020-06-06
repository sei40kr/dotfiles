{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.java.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.java.enable
    # TODO Enable jenv export plugin
    (let jenv = builtins.fetchGit { url = "https://github.com/jenv/jenv.git"; };
    in {
      my = {
        packages = with pkgs; [ jdk11 maven gradle ];
        env = rec {
          JENV_ROOT = jenv.outPath;
          PATH = [ "${JENV_ROOT}/bin" "${JENV_ROOT}/shims" ];
        };
      };
    });
}
