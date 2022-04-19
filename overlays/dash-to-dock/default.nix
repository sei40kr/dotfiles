_self: super: {
  gnomeExtensions = super.gnomeExtensions // {
    dash-to-dock = super.gnomeExtensions.dash-to-dock.overrideAttrs
      ({ patches ? [ ], ... }: {
        patches = patches ++ [ ./fix-shell-version.patch ];
      });
  };
}
