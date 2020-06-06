{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./desktop ./dev ./shell ];

  options.my = {
    env = mkOption {
      type = types.attrs;
    };
    packages = mkOption { type = with types; listOf package; };
  };

  config = {
    my.env = { PATH = [ "$PATH" ]; };

    home = {
      packages = config.my.packages;
      sessionVariables = config.my.env;
    };
  };
}
