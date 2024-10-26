_self: super: {
  gnome = super.gnome // {
    file-roller = super.gnome.file-roller.overrideAttrs (attrs: {
      preFixup = ''
        gappsWrapperArgs+=(
          --prefix PATH : ${super.lib.makeBinPath [ super.p7zip ]}
        )
      '';
    });
  };
}
