{ inputs, perSystem, ... }:
{
  # Minimal home-manager configuration for testing

  # Phase 1: Docker module test
  # Phase 3: Japanese input method test
  # Phase 4: Shell modules test
  # Phase 5: Language modules test (Group 1, Group 2 & Group 3)
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
    inputs.self.homeModules.python
    inputs.self.homeModules.rust
    inputs.self.homeModules.go
    inputs.self.homeModules.java
    inputs.self.homeModules.kotlin
    inputs.self.homeModules.javascript
    inputs.self.homeModules.web
    inputs.self.homeModules.ruby
    inputs.self.homeModules.lua
    inputs.self.homeModules.nix
    inputs.self.homeModules.shell
    inputs.self.homeModules.sql
    inputs.self.homeModules.cc
    inputs.self.homeModules.qml
    inputs.self.homeModules.haskell
    inputs.self.homeModules.julia
    inputs.self.homeModules.latex
    inputs.self.homeModules.lean
    inputs.self.homeModules.prisma
    inputs.self.homeModules.r
    inputs.self.homeModules.solidity
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
  modules.dev.lang.python.enable = true;
  modules.dev.lang.rust.enable = true;
  modules.dev.lang.go.enable = true;
  modules.dev.lang.java.enable = true;
  modules.dev.lang.kotlin.enable = true;
  modules.dev.lang.javascript.enable = true;
  modules.dev.lang.web.enable = true;
  modules.dev.lang.ruby.enable = true;
  modules.dev.lang.lua.enable = true;
  modules.dev.lang.nix.enable = true;
  modules.dev.lang.shell.enable = true;
  modules.dev.lang.sql.enable = true;
  modules.dev.lang.cc.enable = true;
  modules.dev.lang.qml.enable = true;
  modules.dev.lang.haskell.enable = true;
  modules.dev.lang.julia.enable = true;
  modules.dev.lang.latex.enable = true;
  modules.dev.lang.lean.enable = true;
  modules.dev.lang.prisma.enable = true;
  modules.dev.lang.r.enable = true;
  modules.dev.lang.solidity.enable = true;

  home.stateVersion = "23.11";
}
