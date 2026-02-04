pragma Singleton
import QtQuick
import Quickshell.Services.Notifications

QtObject {
    // Called for every notification. Apply transformations here.
    function process(notification: Notification): Notification {
        return notification;
    }
}
