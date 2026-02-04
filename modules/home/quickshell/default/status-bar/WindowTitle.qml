import QtQuick
import Niri 0.1
import "../theme"

Text {
    required property Niri niri

    color: Theme.foregroundColor
    font.pointSize: Theme.fontSizePoints
    font.family: Theme.fontFamily
    text: {
        var title = niri.focusedWindow?.title;
        if (!title) {
            return "";
        }

        return 64 <= title.length ? title.substr(0, 61) + "..." : title;
    }
}
