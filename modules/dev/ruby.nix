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

    modules.shell.zsh.rcInit = ''
      zinit ice wait''' lucid as'completion' id-as'OMZP::gem'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/gem/_gem

      zinit ice wait''' lucid as'completion' id-as'OMZP::rake-fast'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/rake-fast/rake-fast.plugin.zsh
    '';
  };
}
