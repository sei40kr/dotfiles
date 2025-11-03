import QtQuick
import Quickshell

Text {
    required property SystemClock systemClock

    color: Theme.foregroundColor
    font.pointSize: Theme.fontSizePoints
    font.family: Theme.fontFamily
    text: Qt.formatDateTime(systemClock.date, "MMM d ddd  hh:mm")
}
