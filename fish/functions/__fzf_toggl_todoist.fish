# __fzf_toggl_todoist.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function __fzf_toggl_todoist
    if not type -q toggl
        echo 'ERROR: toggl is not installed.' >&2
        return 1
    end

    if not type -q todoist
        echo 'ERROR: todoist is not installed.' >&2
        return 1
    end

    if not type -q fzf
        echo 'ERROR: fzf is not installed.' >&2
        return 1
    end

    set -l todo_item
    todoist --csv list | awk -F, '{ print $6 " " $4 }' | fzf | read todo_item
    and set todo_item (string replace -r '\s+#.+$' '' $todo_item)
    and commandline -- "toggl start '$todo_item'"
    and commandline -f execute
    commandline -f repaint
end
