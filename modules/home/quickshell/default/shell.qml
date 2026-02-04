import QtQuick
import Quickshell
import Niri 0.1
import "status-bar"
import "notifications"

ShellRoot {
    Niri {
        id: niri
        Component.onCompleted: connect()
    }

    StatusBar {
        niri: niri
    }

    NotificationOverlay {
        topGap: 16
        rightGap: 16
    }
}
