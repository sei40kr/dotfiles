{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.ruby.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.ruby.enable (let
    rbenv = builtins.fetchGit { url = "https://github.com/rbenv/rbenv.git"; };
    # TODO Install a rbenv plugin: ruby-build
  in {
    my = rec {
      packages = with pkgs; [ ruby rubyPackages.rake ];
      env = rec {
        RBENV_ROOT = rbenv.outPath;
        PATH = [ "${RBENV_ROOT}/bin" "${RBENV_ROOT}/shims" ];
      };
    };
  });
}
