{ inputs, perSystem, ... }:
{
  # Minimal home-manager configuration for testing

  # Phase 1: Docker module test
  # Phase 3: Japanese input method test
  # Phase 4: Shell modules test
  imports = [
    inputs.self.homeModules.docker
    inputs.self.homeModules.japanese-im
    inputs.self.homeModules.shell-shared
    inputs.self.homeModules.ripgrep
    inputs.self.homeModules.yazi
    inputs.self.homeModules.oj
    inputs.self.homeModules.fastfetch
    inputs.self.homeModules.git
    inputs.self.homeModules.zsh
    inputs.self.homeModules.starship
    inputs.self.homeModules.nushell
  ];

  modules.dev.tools.docker.enable = true;
  modules.shell.enable = true;
  modules.shell.ripgrep.enable = true;
  modules.shell.yazi.enable = true;
  modules.shell.oj.enable = true;
  modules.shell.apps.fastfetch.enable = true;
  modules.shell.git.enable = true;
  modules.shell.zsh.enable = true;
  modules.shell.starship.enable = true;
  modules.shell.nushell.enable = true;

  home.stateVersion = "23.11";
}
