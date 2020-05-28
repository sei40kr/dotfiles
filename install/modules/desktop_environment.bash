# author: Seong Yong-ju <sei40kr@gmail.com>

install_desktop_environment() {
  while true; do
    if is_macos; then
      tui_add_options \
        'HyperDock' install_hyperdock \
        'HyperSwitch' install_hyperswitch
    elif is_archlinux; then
      tui_add_options \
        'Xorg' install_xorg \
        'Picom' install_picom \
        'GNOME' install_gnome \
        'XMonad' install_xmonad \
        'Polybar' install_polybar \
        'Dunst' install_dunst \
        'Fontconfig' install_fontconfig \
        'GTK' install_gtk
    fi
    tui_set_quit_option d 'Done'

    if ! tui_select_options 'Enter your option'; then
      break
    fi
  done
}

install_xorg() {
  assert_archlinux
  pacman_sync dbus xorg xdg-user-dirs xorg-xinit

  ln -fs "${HOME}/.dotfiles/xorg/xprofile" "${HOME}/.xprofile"
  ln -fs "${HOME}/.dotfiles/xorg/xinitrc" "${HOME}/.xinitrc"
  ln -fs "${HOME}/.dotfiles/xorg/Xresources" "${HOME}/.Xresources"
  ln -fs "${HOME}/.dotfiles/xorg/Xmodmap" "${HOME}/.Xmodmap"
  ln -fs "${HOME}/.dotfiles/xorg/xsession" "${HOME}/.xsession"

  LC_ALL=C xdg-user-dirs-update
}

install_picom() {
  assert_archlinux
  pacman_sync picom mesa

  mkdir -p "${XDG_CONFIG_HOME}/picom"
  ln -fs "${HOME}/.dotfiles/picom/picom.conf" \
    "${XDG_CONFIG_HOME}/picom/picom.conf"
}

install_gnome() {
  assert_archlinux
  pacman_sync gnome-flashback \
    gnome-screensaver xautolock \
    gnome-control-center \
    gnome-screenshot

  # Background
  trizen_sync chromecast-wallpapers
  mkdir -p "${HOME}/.local/bin"
  ln -fs "${HOME}/.dotfiles/gnome-desktop/wallpaper.sh" \
    "${HOME}/.local/bin/wallpaper.sh"

  # Night Light
  dconf write /org/gnome/settings-daemon/plugins/color/night-light-enabled \
    true
  dconf write /org/gnome/settings-daemon/plugins/color/night-light-automatic \
    false
  dconf write \
    /org/gnome/settings-daemon/plugins/color/night-light-schedule-from 0
  dconf write \
    /org/gnome/settings-daemon/plugins/color/night-light-schedule-to \
    23.983333333333277
  dconf write \
    /org/gnome/settings-daemon/plugins/color/night-light-temperature 5700

  # Windows
  dconf write /org/gnome/desktop/wm/preferences/button-layout "'appmenu:'"
}

install_xmonad() {
  assert_archlinux
  stack_install xmonad
  pacman_sync xorg-xmessage

  ln -fsT "${HOME}/.dotfiles/xmonad" "${HOME}/.xmonad"
  : >"${HOME}/.xmonad/xmonad.errors"

  # Xmonad session file for use by gnome-session
  mkdir -p "${XDG_CONFIG_HOME}/gnome-session/sessions"
  cat <<FILE >"${XDG_CONFIG_HOME}/gnome-session/sessions/xmonad.session"
[GNOME Session]
Name=GNOME Flashback (Xmonad)
RequiredComponents=gnome-flashback;xmonad;org.gnome.SettingsDaemon.A11ySettings;org.gnome.SettingsDaemon.Color;org.gnome.SettingsDaemon.Datetime;org.gnome.SettingsDaemon.Housekeeping;org.gnome.SettingsDaemon.Keyboard;org.gnome.SettingsDaemon.MediaKeys;org.gnome.SettingsDaemon.Power;org.gnome.SettingsDaemon.PrintNotifications;org.gnome.SettingsDaemon.Rfkill;org.gnome.SettingsDaemon.ScreensaverProxy;org.gnome.SettingsDaemon.Sharing;org.gnome.SettingsDaemon.Smartcard;org.gnome.SettingsDaemon.Sound;org.gnome.SettingsDaemon.Wacom;org.gnome.SettingsDaemon.XSettings;
FILE
  mkdir -p "${XDG_DATA_HOME}/applications"
  cat <<FILE >"${XDG_DATA_HOME}/applications/xmonad.desktop"
[Desktop Entry]
Type=Application
Encoding=UTF-8
Name=Xmonad
Exec=xmonad
NoDisplay=true
X-GNOME-WMName=Xmonad
X-GNOME-Autostart-Phase=WindowManager
X-GNOME-Provides=windowmanager
X-GNOME-Autostart-Notify=false
FILE

  mkdir -p "$XDG_PICTURES_DIR"
}

install_polybar() {
  assert_archlinux
  trizen_sync polybar ttf-material-design-icons-webfont
  pip_install system dbus-python

  mkdir -p "${XDG_CONFIG_HOME}/polybar"
  ln -fs "${HOME}/.dotfiles/polybar/config" "${XDG_CONFIG_HOME}/polybar/config"
  ln -fsT "${HOME}/.dotfiles/polybar/scripts" "${HOME}/polybar-scripts"
}

install_dunst() {
  assert_archlinux
  pacman_sync dunst

  mkdir -p "${XDG_CONFIG_HOME}/dunst"
  ln -fs "${HOME}/.dotfiles/dunst/dunstrc" \
    "${XDG_CONFIG_HOME}/dunst/dunstrc"
}

install_fontconfig() {
  assert_archlinux
  pacman_sync freetype2 fontconfig noto-fonts noto-fonts-cjk noto-fonts-emoji

  sudo tee /etc/profile.d/freetype2.sh <<EOM >/dev/null
# Subpixel hinting mode can be chosen by setting the right TrueType interpreter
# version. The available settings are:
#
#     truetype:interpreter-version=35  # Classic mode (default in 2.6)
#     truetype:interpreter-version=38  # Infinality mode
#     truetype:interpreter-version=40  # Minimal mode (default in 2.7)
#
# There are more properties that can be set, separated by whitespace. Please
# refer to the FreeType documentation for details.

# Uncomment and configure below
export FREETYPE_PROPERTIES='cff:hinting-engine=adobe truetype:interpreter-version=38'
EOM

  mkdir -p "${XDG_CONFIG_HOME}/fontconfig/conf.d"
  cp -f /etc/fonts/conf.avail/10-hinting-none.conf \
    "${XDG_CONFIG_HOME}/fontconfig/conf.d/10-autohint.conf"
  cp -f /etc/fonts/conf.avail/10-sub-pixel-rgb.conf \
    "${XDG_CONFIG_HOME}/fontconfig/conf.d/10-sub-pixel-rgb.conf"
  cp -f /etc/fonts/conf.avail/11-lcdfilter-default.conf \
    "${XDG_CONFIG_HOME}/fontconfig/conf.d/11-lcdfilter-default.conf"
  cp -f /etc/fonts/conf.avail/66-noto-sans.conf \
    "${XDG_CONFIG_HOME}/fontconfig/conf.d/66-noto-sans.conf"
  cp -f /etc/fonts/conf.avail/66-noto-serif.conf \
    "${XDG_CONFIG_HOME}/fontconfig/conf.d/66-noto-serif.conf"
  cp -f /etc/fonts/conf.avail/66-noto-mono.conf \
    "${XDG_CONFIG_HOME}/fontconfig/conf.d/66-noto-mono.conf"
  cp -f /etc/fonts/conf.avail/70-noto-cjk.conf \
    "${XDG_CONFIG_HOME}/fontconfig/conf.d/70-noto-cjk.conf"
}

install_gtk() {
  assert_archlinux
  pacman_sync gtk2 gtk3

  mkdir -p "${XDG_CONFIG_HOME}/gtk-3.0"
  ln -fs "${HOME}/.dotfiles/gtk/gtk-3.0/settings.ini" \
    "${XDG_CONFIG_HOME}/gtk-3.0/settings.ini"
  mkdir -p "${XDG_CONFIG_HOME}/gtk-2.0"
  ln -fs "${HOME}/.dotfiles/gtk/gtk-2.0/gtkrc" \
    "${XDG_CONFIG_HOME}/gtk-2.0/gtkrc"
  ln -fs "${HOME}/.dotfiles/gtk/gtk-2.0/gtkfilechooser.ini" \
    "${XDG_CONFIG_HOME}/gtk-2.0/gtkfilechooser.ini"

  # Install Mac key theme
  mkdir -p "${HOME}/.themes/Mac/gtk-3.0" "${HOME}/.themes/Mac/gtk-2.0-key"
  ln -fs "${HOME}/.dotfiles/gtk/themes/Mac/gtk-3.0/gtk-keys.css" \
    "${HOME}/.themes/Mac/gtk-3.0/gtk-keys.css"
  ln -fs "${HOME}/.dotfiles/gtk/themes/Mac/gtk-2.0-key/gtkrc" \
    "${HOME}/.themes/Mac/gtk-2.0-key/gtkrc"
}

install_hyperdock() {
  assert_macos
  brew_cask_install hyperdock
}

install_hyperswitch() {
  assert_macos
  brew_cask_install hyperswitch
}
