{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.tools.atcoderTools.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.atcoderTools.enable {
    my.packages = with pkgs; [ my.python3Packages.atcoder-tools ];
    my.home.home.file = {
      ".atcodertools.toml".source = <config/atcoder-tools/atcodertools.toml>;
      "custom_code_generator.py".source =
        <config/atcoder-tools/custom_code_generator.py>;
      "my_template.rs".source = <config/atcoder-tools/my_template.rs>;
    };
  };
}
