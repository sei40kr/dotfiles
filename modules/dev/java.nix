{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.java.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.java.enable (let
    jenv = builtins.fetchGit { url = "https://github.com/jenv/jenv.git"; };
    jenvRootFiles =
      [ "available-plugins" "bin" "completions" "fish" "libexec" ];
    jenvPlugins = [ "export" ];
  in {
    my = {
      home.home.file = (foldl (files: name:
        files // {
          ".jenv/${name}".source = "${jenv.outPath}/${name}";
        }) { } jenvRootFiles) // (foldl (files: name:
          files // {
            ".jenv/plugins/${name}".source =
              "${jenv.outPath}/available-plugins/${name}";
          }) { } jenvPlugins);

      packages = with pkgs; [ jdk11 maven gradle ];
      env = rec {
        JENV_ROOT = "\${HOME}/.jenv";
        PATH = [ "${JENV_ROOT}/bin" "${JENV_ROOT}/shims" ];
      };
    };
  });
}
