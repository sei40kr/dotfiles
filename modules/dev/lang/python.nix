{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.python;

  patchedPython3 = pkgs.symlinkJoin {
    name = "${pkgs.python3.name}-patched";
    paths = [ pkgs.python3 ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/python3.12 \
        --prefix LD_LIBRARY_PATH : "${makeLibraryPath config.programs.nix-ld.libraries}"
    '';
  };
in
{
  options.modules.dev.lang.python = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      patchedPython3
      uv
      basedpyright
      ruff
    ];
    env.PATH = [ "\${HOME}/.poetry/bin" ];

    modules.shell.zsh.rcInit = ''
      zinit ice wait''' lucid as'completion' id-as'OMZP::pip'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/pip/_pip
    '';
  };
}
