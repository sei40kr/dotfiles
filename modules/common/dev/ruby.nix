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

      zsh.zinit.snippets = [
        {
          source =
            "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/ruby/ruby.plugin.zsh";
          ice.id-as = "OMZP::ruby";
        }
        {
          source = "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/gem/_gem";
          ice = {
            wait = "";
            lucid = true;
            as = "completion";
            id-as = "OMZP::gem";
          };
        }
        {
          source =
            "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/rake-fast/rake-fast.plugin.zsh";
          ice = {
            wait = "";
            lucid = true;
            id-as = "OMZP::rake-fast";
          };
        }
      ] ++ (optionals cfg.rails.enable [{
        source =
          "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/rails/rails.plugin.zsh";
        ice.id-as = "OMZP::rails";
      }]);
    };
  };
}
