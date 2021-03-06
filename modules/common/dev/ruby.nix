{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.ruby;
in {
  options.modules.dev.ruby = {
    enable = mkBoolOpt false;
    rails.enable = mkBoolOpt false;
  };

  config = mkIf config.modules.dev.ruby.enable {
    user.packages = with pkgs;
      ([ ruby rubocop rubyPackages.rake solargraph ]
        ++ optionals cfg.rails.enable [ rubyPackages.rails ]);
    modules.shell = {
      aliases = {
        be = "bundle exec";
        bl = "bundle list";
        bp = "bundle package";
        bo = "bundle open";
        bout = "bundle outdated";
        bu = "bundle update";
        bi = "bundle_install";
        bcn = "bundle clean";
      };
      zsh.extraZinitCommands = ''
        zinit snippet OMZP::ruby/ruby.plugin.zsh
        zinit ice as'completion' wait'''
        zinit snippet OMZP::gem/_gem
        zinit ice wait'''
        zinit snippet OMZP::rake-fast/rake-fast.plugin.zsh
      '' + optionalString cfg.rails.enable ''
        zinit snippet OMZP::rails/rails.plugin.zsh
      '';
    };
  };
}
