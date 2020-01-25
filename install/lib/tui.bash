# tui.bash
# author: Seong Yong-ju

ask-yesno() {
    local prompt="$1"

    tui-print-prompt "${prompt} [yn]"

    while true; do
        read -n1 -r answer

        if [[ "$answer" == [yY]* ]]; then
            return 0
        elif [[ "$answer" == [nN]* ]]; then
            return 1
        else
            echo
            tui-print-prompt "Invalid answer. ${prompt} [yn]"
        fi
    done
}
