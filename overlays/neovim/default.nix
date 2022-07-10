_self: super:
{
  neovim = super.neovim.overrideAttrs ({ buildCommand, ... }: {
    buildCommand = ''
      ${buildCommand}
      rm -f $out/share/applications/nvim.desktop
    '';
  });
}
