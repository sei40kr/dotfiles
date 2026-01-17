import QtQuick
import QtQuick.Layouts

RowLayout {
    required property var workspaces

    spacing: 16

    Repeater {
        model: workspaces
        delegate: Rectangle {
            required property var modelData

            color: modelData.isActive ? Theme.activeColor : Theme.inactiveColor
            height: 6
            radius: 6
            scale: modelData.isActive ? 2.0 : 1.0
            width: 6
        }
    }
}
