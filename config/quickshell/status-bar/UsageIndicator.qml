import QtQuick

Rectangle {
    id: root

    required property real percentage  // 0-100

    width: 40
    height: 4
    color: Theme.usageBarBackground

    Rectangle {
        id: barFill
        anchors {
            left: parent.left
            top: parent.top
            bottom: parent.bottom
        }
        width: Math.max(0, Math.min(parent.width * (root.percentage / 100), parent.width))
        color: {
            if (root.percentage >= 85) {
                return Theme.usageDanger;
            } else if (root.percentage >= 60) {
                return Theme.usageWarning;
            } else {
                return Theme.usageSafe;
            }
        }

        Behavior on width {
            NumberAnimation {
                duration: 50
                easing.type: Easing.OutCubic
            }
        }

        Behavior on color {
            ColorAnimation {
                duration: 50
            }
        }
    }
}
