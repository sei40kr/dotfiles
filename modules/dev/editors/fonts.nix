{ config, lib, options, pkgs, ... }:

with lib; {
  config.my.packages = with pkgs; [ fira-code source-code-pro jetbrains-mono ];
}
