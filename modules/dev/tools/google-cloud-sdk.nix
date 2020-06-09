{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.googleCloudSdk.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.googleCloudSdk.enable {
    my.packages = with pkgs; [ google-cloud-sdk ];
  };
}
