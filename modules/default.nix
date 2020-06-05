{ config, lib, options, pkgs, ... }:

with lib; {
  config.home.sessionVariables.PATH = [ "\$PATH" ];

  imports = [ ./dev ./shell ];
}
