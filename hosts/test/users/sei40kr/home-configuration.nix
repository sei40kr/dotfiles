{ inputs, perSystem, ... }:
{
  # Minimal home-manager configuration for testing

  # Phase 1: Docker module test
  # Phase 3: Japanese input method test
  imports = [
    inputs.self.homeModules.docker
    inputs.self.homeModules.japanese-im
  ];

  modules.dev.tools.docker.enable = true;

  home.stateVersion = "23.11";
}
