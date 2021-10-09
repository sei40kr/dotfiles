{ config, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.shell.atcoder-tools;
  configFile = config:
    with pkgs;
    runCommand ".atcodertools.toml" {
      buildInputs = [ remarshal ];
      preferLocalBuild = true;
      allowSubstitutes = false;
    } ''
      remarshal -if json -of toml \
        <${pkgs.writeText "config.json" (toJSON config)} >"$out"
    '';
in {
  options.modules.shell.atcoder-tools = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ my.python3Packages.atcoder-tools ];
    home.file.".atcodertools.toml".source = configFile {
      codestyle = {
        indent_type = "space";
        indent_width = 4;
        template_file = "${configDir}/atcoder-tools/my_template.rs";
        workspace_dir = "~/dev/ws/sei40kr/hello-atcoder/";
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
