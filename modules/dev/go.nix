{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.go.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.go.enable (let
    goenv = builtins.fetchGit { url = "https://github.com/syndbg/goenv.git"; };
  in {
    my = {
      packages = with pkgs; [ go ];
      env = rec {
        GOENV_ROOT = goenv.outPath;
        PATH = [ "${GOENV_ROOT}/bin" "${GOENV_ROOT}/shims" ];
      };
    };
  });
}
