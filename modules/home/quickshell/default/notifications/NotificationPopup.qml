import QtQuick
import QtQuick.Layouts
import Quickshell.Services.Notifications
import "../theme"

Item {
    id: root

    property Notification notification
    property int count
    property date createdAt

    signal dismissed

    readonly property color accentColor: notification.urgency === NotificationUrgency.Critical ? Theme.dangerColor : notification.urgency === NotificationUrgency.Low ? Theme.mutedColor : Theme.infoColor

    width: 300
    height: background.height
    opacity: 0

    Rectangle {
        id: background
        width: parent.width
        height: mainLayout.implicitHeight + 24
        color: mouseArea.pressed ? Qt.darker(Theme.notificationBackground, 1.3) : hoverHandler.hovered ? Qt.lighter(Theme.notificationBackground, 1.3) : Theme.notificationBackground

        Rectangle {
            id: accentBorder
            anchors.left: parent.left
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            width: 3
            color: root.accentColor
        }

        ColumnLayout {
            id: mainLayout

            anchors {
                fill: parent
                leftMargin: 20
                rightMargin: 12
                topMargin: 12
                bottomMargin: 12
            }
            spacing: 8

            RowLayout {
                Layout.fillWidth: true

                Text {
                    Layout.fillWidth: true
                    text: root.notification.summary + (root.count > 1 ? " (%1)".arg(root.count) : "")
                    color: Theme.foregroundColor
                    font.bold: true
                    font.family: Theme.fontFamily
                    font.pointSize: Theme.smallFontSizePoints
                    elide: Text.ElideRight
                }

                Text {
                    id: timeLabel
                    color: Theme.inactiveColor
                    font.family: Theme.fontFamily
                    font.pointSize: Theme.smallFontSizePoints
                    text: root.humanizeTime()
                }
            }

            Text {
                Layout.fillWidth: true
                text: root.notification.body
                color: Theme.notificationBody
                font.family: Theme.fontFamily
                font.pointSize: Theme.smallFontSizePoints
                wrapMode: Text.Wrap
                maximumLineCount: 2
                elide: Text.ElideRight
                visible: text !== ""
            }
        }

        HoverHandler {
            id: hoverHandler
            cursorShape: Qt.PointingHandCursor
        }

        MouseArea {
            id: mouseArea
            anchors.fill: parent
            onClicked: root.dismiss()
        }
    }

    Timer {
        id: timeUpdateTimer
        interval: 30000
        repeat: true
        running: true
        onTriggered: timeLabel.text = root.humanizeTime()
    }

    function humanizeTime(): string {
        const seconds = Math.floor((new Date() - root.createdAt) / 1000);
        if (seconds < 60)
            return "now";
        const minutes = Math.floor(seconds / 60);
        if (minutes < 60)
            return minutes + "m ago";
        const hours = Math.floor(minutes / 60);
        return hours + "h ago";
    }

    Component.onCompleted: fadeIn.start()

    NumberAnimation {
        id: fadeIn
        target: root
        property: "opacity"
        from: 0
        to: 1
        duration: 150
    }

    function dismiss() {
        notification?.dismiss();
        root.dismissed();
    }
}
