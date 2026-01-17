import QtQuick
import Quickshell
import Niri 0.1
import "status-bar"

ShellRoot {
    Niri {
        id: niri
        Component.onCompleted: connect()
    }

    StatusBar {
        niri: niri
    }
}
