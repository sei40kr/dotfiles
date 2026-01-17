{
  inputs,
  perSystem,
  pkgs,
  ...
}:
{
  imports = [ inputs.self.homeModules.home-shared ];

  # Enable automatic garbage collection
  # NOTE: This will only garbage collect the user's profiles
  nix.gc = {
    automatic = true;
    dates = ''
      *-*-* 03:00:00
    '';
  };

  # Docker
  modules.dev.tools.docker.enable = true;

  # Shell
  modules.shell.enable = true;
  modules.shell.ripgrep.enable = true;
  modules.shell.yazi.enable = true;
  modules.shell.oj.enable = true;
  modules.shell.apps.fastfetch.enable = true;
  modules.shell.git.enable = true;
  modules.shell.zsh.enable = true;
  modules.shell.starship.enable = true;
  modules.shell.nushell.enable = true;

  # Dev Lang
  modules.dev.lang.python.enable = true;
  modules.dev.lang.rust.enable = true;
  modules.dev.lang.go.enable = true;
  modules.dev.lang.java.enable = true;
  modules.dev.lang.kotlin.enable = true;
  modules.dev.lang.javascript.enable = true;
  modules.dev.lang.web.enable = true;
  modules.dev.lang.web.bun.enable = true;
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

  # Dev Tools
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

  # AI
  modules.ai.agent-browser.enable = true;
  modules.ai.claude-code.enable = true;
  modules.ai.claude-code.ccstatusline.enable = true;
  modules.ai.codex.enable = true;
  modules.ai.crush.enable = true;
  modules.ai.gemini-cli.enable = true;

  # Term
  modules.term.kitty.enable = true;
  modules.term.wezterm.enable = true;
  modules.term.sensible.enable = true;
  modules.term.colorschemes.active = "tokyo-night";
  modules.term.font = {
    package = perSystem.self.julia-mono-nf;
    name = "JuliaMono Nerd Font";
    size = 18;
  };

  # Editors
  modules.editors.helix.enable = true;
  modules.editors.lazyvim.enable = true;
  modules.editors.ideavim.enable = true;
  modules.editors.ideavim.doom.enable = true;
  modules.editors.zed.enable = true;
  modules.editors.emacs.enable = true;
  modules.editors.emacs.doom.enable = true;
  modules.editors.emacs.doom.theme = "doom-tokyo-night";
  modules.editors.idea.enable = true;
  modules.editors.fonts.code = {
    package = pkgs.nerd-fonts.iosevka;
    name = "Iosevka NFM";
    size = 17;
  };

  # Desktop Media
  modules.desktop.media.inkscape.enable = true;
  modules.desktop.media.gimp.enable = true;
  modules.desktop.media.obs-studio.enable = true;
  modules.desktop.media.zathura.enable = true;
  modules.desktop.media.foliate.enable = true;

  # Desktop Apps
  modules.desktop.apps.bitwarden.enable = true;
  modules.desktop.apps.anyrun.enable = true;
  modules.desktop.apps.dunst.enable = true;
  modules.desktop.apps.gnome.pomodoro.enable = true;
  modules.desktop.apps.quickshell.enable = true;
  modules.desktop.apps.swww.enable = true;

  # Desktop Browsers
  modules.desktop.browsers.firefox.enable = true;

  # Temporary packages (not yet migrated to modules)
  home.packages = with pkgs; [
    vlc
  ];

  home.stateVersion = "23.11";
}
