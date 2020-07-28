{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.ruby;
in {
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

  config = mkIf config.modules.dev.ruby.enable {
    my.packages = with pkgs;
      ([ ruby rubyPackages.rake ]
        ++ optionals cfg.enableRails [ rubyPackages.rails ]);
    my.aliases = {
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
      zinit snippet OMZP::ruby/ruby.plugin.zsh
      zinit ice as'completion' wait'''
      zinit snippet OMZP::gem/_gem
      zinit ice wait'''
      zinit snippet OMZP::rake-fast/rake-fast.plugin.zsh
    '' + optionals cfg.enableRails ''
      zinit snippet OMZP::rails/rails.plugin.zsh
    '';
  };
}
