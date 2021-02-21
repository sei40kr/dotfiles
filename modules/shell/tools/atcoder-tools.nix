{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.shell.tools.atcoderTools.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.atcoderTools.enable {
    user.packages = with pkgs; [ my.python3Packages.atcoder-tools ];
    home.file.".atcodertools.toml".text = generators.toINI { } {
      codestyle = {
        indent_type = "space";
        indent_width = 4;
        template_file = "${configDir}/atcoder-tools/my_template.rs";
        workspace_dir = "~/projects/sei40kr/hello-atcoder/";
        lang = "rust";
        code_generator_file =
          "${configDir}/atcoder-tools/custom_code_generator.py";
      };

      postprocess = {
        exec_on_each_problem_dir = "rustfmt -q main.rs; cargo init -q";
      };

      etc = {
        download_without_login = false;
        parallel_download = false;
        save_no_session_cache = false;
      };
    };
  };
}
