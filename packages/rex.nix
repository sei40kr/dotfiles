{ perSystem, ... }:

perSystem.rex.rex.overrideAttrs (old: {
  # Rex writes its agent skill only through `rex skill setup`, so run it here
  # to ship the skill in the package, as nixpkgs does for agent skills.
  postInstall = (old.postInstall or "") + ''
    HOME=$TMPDIR $out/bin/rex skill setup --agent claude --autostart=false
    mkdir -p $out/share/skills/rex
    cp -r $TMPDIR/.claude/skills/rex $out/share/skills/rex/
  '';
})
