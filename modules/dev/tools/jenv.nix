{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.jenv.enable = mkOption {
    type = types.bool;
    default = false;
  };

  options.modules.dev.tools.jenv.pluginsToEnable = mkOption {
    type = with types; listOf str;
    default = [ "export" ];
  };

  config = mkIf config.modules.dev.tools.jenv.enable (let
    jenv = builtins.fetchGit { url = "https://github.com/jenv/jenv.git"; };
    jenvRootFiles =
      [ "available-plugins" "bin" "completions" "fish" "libexec" ];
  in {
    my = {
      home.home.file = (foldl (files: name:
        files // {
          ".jenv/${name}".source = "${jenv.outPath}/${name}";
        }) { } jenvRootFiles) // (foldl (files: name:
          files // {
            ".jenv/plugins/${name}".source =
              "${jenv.outPath}/available-plugins/${name}";
          }) { } config.modules.dev.tools.jenv.pluginsToEnable);

      env = rec {
        JENV_ROOT = "\${HOME}/.jenv";
        PATH = [ "${JENV_ROOT}/bin" "${JENV_ROOT}/shims" ];
      };
    };

    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice atclone'jenv init - --no-rehash zsh >jenv-init.zsh' \
                atpull'%atclone' \
                id-as'jenv-init'
      zinit light zdharma/null
    '';
  });
}
