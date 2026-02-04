import QtQuick
import QtQuick.Effects
import Quickshell
import Quickshell.Wayland
import Quickshell.Services.Notifications
import "../theme"

PanelWindow {
    id: root

    required property int topGap
    required property int rightGap

    WlrLayershell.layer: WlrLayer.Overlay
    WlrLayershell.namespace: "shell:notifications"

    exclusionMode: ExclusionMode.Ignore
    anchors {
        top: true
        left: true
        right: true
        bottom: true
    }

    color: "transparent"
    mask: Region {
        item: notificationContainer
    }
    visible: notifications.count > 0

    ListModel {
        id: notifications
    }

    function findDuplicate(notification: Notification): int {
        for (let i = 0; i < notifications.count; i++) {
            const item = notifications.get(i);
            if (item.notification.summary === notification.summary && item.notification.body === notification.body) {
                return i;
            }
        }
        return -1;
    }

    function removeNotification(index: int) {
        if (index >= 0 && index < notifications.count) {
            notifications.remove(index);
        }
    }

    NotificationServer {
        id: notificationServer

        bodyMarkupSupported: true
        actionsSupported: false
        imageSupported: true
        persistenceSupported: true

        onNotification: notification => {
            notification.tracked = true;
            NotificationRules.process(notification);

            const existingIndex = root.findDuplicate(notification);
            if (existingIndex >= 0) {
                const existing = notifications.get(existingIndex);
                notifications.set(existingIndex, {
                    notification: notification,
                    count: existing.count + 1,
                    createdAt: new Date()
                });
                if (existingIndex > 0) {
                    notifications.move(existingIndex, 0, 1);
                }
            } else {
                notifications.insert(0, {
                    notification: notification,
                    count: 1,
                    createdAt: new Date()
                });
            }
        }
    }

    Rectangle {
        id: notificationMask
        width: notificationContainer.width
        height: notificationContainer.height
        radius: 8
        visible: false
        layer.enabled: true
    }

    Rectangle {
        id: notificationContainer

        anchors {
            top: parent.top
            right: parent.right
            topMargin: Theme.statusBarHeight + root.topGap
            rightMargin: root.rightGap
        }

        width: notificationColumn.width
        height: notificationColumn.height
        color: "transparent"

        layer.enabled: notifications.count > 0
        layer.effect: MultiEffect {
            maskEnabled: true
            maskSource: notificationMask
            maskThresholdMin: 0.5
            maskSpreadAtMin: 0.5
        }

        Column {
            id: notificationColumn
            spacing: 0

            Repeater {
                model: notifications

                delegate: Column {
                    required property int index
                    required property var model

                    Rectangle {
                        width: popup.width
                        height: index === 0 ? 0 : 1
                        color: Theme.notificationSeparator
                    }

                    NotificationPopup {
                        id: popup

                        notification: model.notification
                        count: model.count
                        createdAt: model.createdAt

                        onDismissed: root.removeNotification(index)
                    }
                }
            }
        }
    }
}
