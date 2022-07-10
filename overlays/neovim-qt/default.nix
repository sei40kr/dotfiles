_self: super:
{
  neovim-qt = super.neovim-qt.overrideAttrs ({ buildCommand, ... }: {
    buildCommand = ''
      ${buildCommand}
      rm -r $out/share/applications
      mkdir -p $out/share/applications
      cp ${super.neovim-qt-unwrapped}/share/applications/nvim-qt.desktop \
         $out/share/applications
      sed -i -e '/\[Desktop Entry\]/a\' -e 'StartupWMClass=.nvim-qt-wrapped' \
          $out/share/applications/nvim-qt.desktop
    '';
  });
}
