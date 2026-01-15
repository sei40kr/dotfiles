{ inputs, perSystem, ... }:
{
  # Minimal home-manager configuration for testing

  # Phase 1: Docker module test
  imports = [
    inputs.self.homeModules.docker
  ];

  modules.dev.tools.docker.enable = true;

  home.stateVersion = "23.11";
}
