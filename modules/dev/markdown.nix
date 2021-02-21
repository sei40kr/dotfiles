{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.markdown.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.markdown.enable {
    user.packages = with pkgs; [
      # TODO markdownlint-cli
      mdl
      python37Packages.grip
    ];
  };
}
