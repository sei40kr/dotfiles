{ inputs, perSystem, ... }:
{
  # Minimal home-manager configuration for testing

  # Phase 1: Docker module test
  # Phase 3: Japanese input method test
  # Phase 4: Shell modules test
  # Phase 5: Language modules test (Group 1, Group 2 & Group 3)
  # Phase 7a: AI modules test
  # Phase 7b: Term modules test
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
    inputs.self.homeModules.ansible
    inputs.self.homeModules.aws
    inputs.self.homeModules.azure
    inputs.self.homeModules.difftastic
    inputs.self.homeModules.github
    inputs.self.homeModules.gitu
    inputs.self.homeModules.jupyter
    inputs.self.homeModules.k8s
    inputs.self.homeModules.terraform
    inputs.self.homeModules.agent-browser
    inputs.self.homeModules.claude-code
    inputs.self.homeModules.codex
    inputs.self.homeModules.crush
    inputs.self.homeModules.gemini-cli
    inputs.self.homeModules.kitty
    inputs.self.homeModules.wezterm
    inputs.self.homeModules.sensible-terminal
    inputs.self.homeModules.tokyo-night
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

  modules.dev.tools.ansible.enable = true;
  modules.dev.tools.aws.enable = true;
  modules.dev.tools.aws.cfn.enable = true;
  modules.dev.tools.aws.copilot.enable = true;
  # modules.dev.tools.aws.sam.enable = true; # TODO: aws-sam-cli is broken
  modules.dev.tools.azure.enable = true;
  modules.dev.tools.azure.kubelogin.enable = true;
  modules.dev.tools.difftastic.enable = true;
  modules.dev.tools.github.enable = true;
  modules.dev.tools.gitu.enable = true;
  modules.dev.tools.jupyter.enable = true;
  modules.dev.tools.k8s.enable = true;
  modules.dev.tools.k8s.helm.enable = true;
  modules.dev.tools.terraform.enable = true;

  modules.ai.agent-browser.enable = true;
  modules.ai.claude-code.enable = true;
  modules.ai.claude-code.ccstatusline.enable = true;
  modules.ai.codex.enable = true;
  modules.ai.crush.enable = true;
  modules.ai.gemini-cli.enable = true;

  modules.term.kitty.enable = true;
  modules.term.wezterm.enable = true;
  modules.term.sensible.enable = true;
  modules.term.colorschemes.active = "tokyo-night";

  home.stateVersion = "23.11";
}
