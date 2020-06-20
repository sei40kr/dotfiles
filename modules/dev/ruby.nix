{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.ruby = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableRails = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.dev.ruby.enable (let
    cfg = config.modules.dev.ruby;
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

      packages = with pkgs;
        ([
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
        ] ++ optionals cfg.enableRails [ rubyPackages.rails ]);
      env = rec {
        RBENV_ROOT = "\${HOME}/.rbenv";
        PATH = [ "${RBENV_ROOT}/bin" "${RBENV_ROOT}/shims" ];
      };
    };

    my.zsh.aliases = {
      be = "bundle exec";
      bl = "bundle list";
      bp = "bundle package";
      bo = "bundle open";
      bout = "bundle outdated";
      bu = "bundle update";
      bi = "bundle_install";
      bcn = "bundle clean";
    };
    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice atclone'rbenv init - --no-rehash zsh >rbenv-init.zsh' \
                atpull'%atclone' \
                id-as'rbenv-init'
      zinit light zdharma/null

      zinit snippet OMZP::ruby/ruby.plugin.zsh
      zinit ice as'completion' wait'''
      zinit snippet OMZP::gem/_gem
      zinit ice wait'''
      zinit snippet OMZP::rake-fast/rake-fast.plugin.zsh
    '' + optionals cfg.enableRails ''
      zinit snippet OMZP::rails/rails.plugin.zsh
    '';
  });
}
