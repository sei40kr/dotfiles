{ lib
, makeWrapper
, runCommandLocal
, symlinkJoin
, sysstat
, tmux
, tmuxPlugins
}:

let
  doom-statusline = runCommandLocal "doom-statusline.tmux" { } ''
    cp ${./doom-statusline.tmux} $out
    patchShebangs --build $out
  '';
  cfg = runCommandLocal "tmux.conf" { } ''
    substitute ${./tmux.conf} $out \
      --subst-var-by copycat          ${tmuxPlugins.copycat.rtp} \
      --subst-var-by cowboy           ${tmuxPlugins.cowboy.rtp} \
      --subst-var-by cpu              ${tmuxPlugins.cpu.rtp} \
      --subst-var-by doom_statusline  ${doom-statusline} \
      --subst-var-by ghq              ${tmuxPlugins.ghq.rtp} \
      --subst-var-by online_status    ${tmuxPlugins.online-status.rtp} \
      --subst-var-by open             ${tmuxPlugins.open.rtp} \
      --subst-var-by pain_control     ${tmuxPlugins.pain-control.rtp} \
      --subst-var-by prefix_highlight ${tmuxPlugins.prefix-highlight.rtp} \
      --subst-var-by sessionist       ${tmuxPlugins.sessionist.rtp} \
      --subst-var-by urlview          ${tmuxPlugins.urlview.rtp} \
      --subst-var-by yank             ${tmuxPlugins.yank.rtp}
  '';
in
symlinkJoin {
  name = "yonmux";

  paths = [ tmux ];
  nativeBuildInputs = [ makeWrapper ];

  postBuild = ''
    wrapProgram $out/bin/tmux --set __ETC_BASHRC_SOURCED "" \
                              --set __ETC_ZSHENV_SOURCED "" \
                              --set __ETC_ZPROFILE_SOURCED  "" \
                              --set __ETC_ZSHRC_SOURCED "" \
                              --set __NIX_SET_ENVIRONMENT_DONE "" \
                              --set __NIX_DARWIN_SET_ENVIRONMENT_DONE "" \
                              --prefix PATH : ${lib.makeBinPath [ sysstat ]} \
                              --add-flags "-f ${cfg}"
  '';
}

