import QtQuick
import QtQuick.Layouts
import "../theme"

RowLayout {
    id: root
    spacing: 8
    visible: service.state !== GnomePomodoroService.State.Stopped

    GnomePomodoroService {
        id: service
    }

    function formatTime(totalSeconds) {
        const minutes = Math.floor(totalSeconds / 60);
        const seconds = totalSeconds % 60;
        return String(minutes).padStart(2, '0') + ':' + String(seconds).padStart(2, '0');
    }

    function getIconName() {
        if (service.isPaused) {
            return 'pause';
        }

        switch (service.state) {
        case GnomePomodoroService.State.Pomodoro:
            return 'tomato';
        case GnomePomodoroService.State.ShortBreak:
            return 'coffee';
        case GnomePomodoroService.State.LongBreak:
            return 'bed';
        default:
            return '';
        }
    }

    NFIcon {
        name: getIconName()
    }

    Text {
        text: formatTime(service.remainingSeconds)
        color: Theme.foregroundColor
        font.family: Theme.fontFamily
        font.pointSize: Theme.fontSizePoints
    }
}
