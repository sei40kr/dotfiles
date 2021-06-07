{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.travis;
in {
  options.modules.dev.travis = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ travis ];

    modules.shell.zsh.zinit.snippets = [{
      source = "\${HOME}/.travis/travis.sh";
      ice = {
        "if" = ''[[ -f "''${HOME}/.travis/travis.sh" ]]'';
        wait = "";
        lucid = true;
      };
    }];
  };
}
