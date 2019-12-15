#!/usr/bin/env python
# coding: utf-8
"""
Fcitx Status Indicator for Polybar
"""

__author__ = """Seong Yong-ju <sei40kr@gmail.com>"""

import subprocess

from gi.repository import GLib
import dbus
import dbus.mainloop.glib


class Status:
    def __init__(self, is_running: bool, is_active: bool):
        self.is_running = False
        self.is_active = False


def get_status_code() -> int:
    try:
        raw_status_code = subprocess.check_output('fcitx-remote')
    except subprocess.CalledProcessError:
        return 0
    return int(raw_status_code)


def output(is_running: bool, is_active: bool):
    if is_running:
        print('%{O3}あ%{O2}' if is_active else 'Aa', flush=True)


def main():
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

    status = Status(False, False)

    def update_status():
        status_code = get_status_code()

        status.is_running = status_code != 0
        status.is_active = status_code == 2

    def dbus_signal_handler(*args, **kwargs):
        update_status()

        output(is_running=status.is_running, is_active=status.is_active)

    bus = dbus.SessionBus()
    bus.add_signal_receiver(dbus_signal_handler,
                            dbus_interface='org.freedesktop.DBus.Properties',
                            path='/inputmethod',
                            member_keyword='PropertiesChanged')

    # initial rendering
    update_status()
    output(is_running=status.is_running, is_active=status.is_active)

    loop = GLib.MainLoop()
    loop.run()


if __name__ == '__main__':
    main()
