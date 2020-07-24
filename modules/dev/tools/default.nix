{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./ansible.nix
    ./aws-cli.nix
    ./aws-shell.nix
    ./circleci-cli.nix
    ./datagrip.nix
    ./docker-compose.nix
    ./git.nix
    ./google-cloud-sdk.nix
    ./gradle.nix
    ./jenv.nix
    ./maven.nix
    ./mycli.nix
    ./pgcli.nix
    ./spring-boot.nix
    ./streamlit.nix
    ./travis.nix
    ./zeal.nix
  ];
}
