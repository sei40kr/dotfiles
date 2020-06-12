{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.ruby.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.ruby.enable (let
    rbenv = builtins.fetchGit { url = "https://github.com/rbenv/rbenv.git"; };
    rbenvRootFiles = [ "bin" "completions" "libexec" "rbenv.d" "src" ];
    rubyBuild =
      builtins.fetchGit { url = "https://github.com/rbenv/ruby-build.git"; };
  in {
    my = {
      home.home.file = foldl (files: name:
        files // {
          ".rbenv/${name}".source = "${rbenv.outPath}/${name}";
        }) { } rbenvRootFiles // {
          ".rbenv/plugins/ruby-build".source = rubyBuild.outPath;
        };

      packages = with pkgs; [
        ruby
        rubyPackages.rake

        # NOTE rbenv: Ruby build environment
        #      See https://github.com/rbenv/ruby-build/wiki#suggested-build-environment
        gcc10
        bzip2
        openssl
        libyaml
        libffi
        readline
        zlib
      ];
      env = rec {
        RBENV_ROOT = "\${HOME}/.rbenv";
        PATH = [ "${RBENV_ROOT}/bin" "${RBENV_ROOT}/shims" ];
      };
    };
  });
}
