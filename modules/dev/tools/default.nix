{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./aws-shell.nix
    ./circleci-cli.nix
    ./git.nix
    ./google-cloud-sdk.nix
    ./travis.nix
    ./zeal.nix
  ];
}
