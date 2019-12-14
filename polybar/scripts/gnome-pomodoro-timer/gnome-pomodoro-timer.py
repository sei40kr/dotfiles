#!/usr/bin/env python
# coding: utf-8
"""
GNOME Pomodoro Timer for Polybar
"""

__author__ = """Seong Yong-ju <sei40kr@gmail.com>"""

from typing import Optional

from gi.repository import Gio, GLib
import dbus
import dbus.mainloop.glib


class Timer:
    def __init__(self, state: Optional[str], state_duration: int,
                 elapsed: float, is_paused: bool):
        self.state = state
        self.state_duration = state_duration
        self.elapsed = elapsed
        self.is_paused = is_paused


def format_icon(state: Optional[str]):
    icon = '' if state is not None else '%{F#666666}%{F-}'
    return f'%{{T2}}{icon}%{{T-}}'


def format_time(duration: int, elapsed: float):
    rest = duration - int(elapsed)
    return f'{int(rest/60)}:{rest%60:02}'


def output(state: Optional[str], state_duration: int, elapsed: float,
           is_paused: bool):
    print(format_icon(state), end='')
    if state is not None:
        print(
            f'%{{O8}}{format_time(duration=state_duration, elapsed=elapsed)}',
            end='')
    print('', flush=True)


def main():
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

    gsettings = Gio.Settings.new(schema_id='org.gnome.pomodoro.state')
    raw_state = Gio.Settings.get_string(gsettings, 'timer-state')
    state = raw_state if raw_state != 'null' else None
    state_duration = int(
        Gio.Settings.get_double(gsettings, 'timer-state-duration') or 0.0)
    elapsed = Gio.Settings.get_double(gsettings, 'timer-elapsed') or 0.0
    is_paused = Gio.Settings.get_boolean(gsettings, 'timer-paused') or False

    timer = Timer(state=state,
                  state_duration=state_duration,
                  elapsed=elapsed,
                  is_paused=is_paused)

    def dbus_signal_handler(*args, **kwargs):
        payload = args[1]

        if 'State' in payload:
            timer.state = str(
                payload['State']) if payload['State'] != 'null' else None
            if timer.state is None:
                timer.is_paused = False
        if 'StateDuration' in payload:
            timer.state_duration = int(payload['StateDuration'])
        if 'Elapsed' in payload:
            timer.elapsed = float(payload['Elapsed'])

        if 'IsPaused' in payload:
            timer.is_paused = bool(payload['IsPaused'])

        output(state=timer.state,
               state_duration=timer.state_duration,
               elapsed=timer.elapsed,
               is_paused=timer.is_paused)

    bus = dbus.SessionBus()
    bus.add_signal_receiver(dbus_signal_handler,
                            dbus_interface='org.freedesktop.DBus.Properties',
                            path='/org/gnome/Pomodoro',
                            member_keyword='PropertiesChanged')

    # initial rendering
    output(state=timer.state,
           state_duration=timer.state_duration,
           elapsed=timer.elapsed,
           is_paused=timer.is_paused)

    loop = GLib.MainLoop()
    loop.run()


if __name__ == '__main__':
    main()
