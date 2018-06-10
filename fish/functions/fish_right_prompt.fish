# fish_right_prompt.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function fish_right_prompt
    set -l __bobthefish_left_arrow_glyph \uE0B3
    if [ "$theme_powerline_fonts" = "no" ]
        set __bobthefish_left_arrow_glyph '<'
    end

    set_color $fish_color_autosuggestion

    if not type -q toggl
        __bobthefish_cmd_duration
        __bobthefish_timestamp
    else
        set -l toggl_desc
        set -l toggl_duration
        for line in (toggl --cache --csv current)
            set -l kv (string split , $line)

            switch $kv[1]
                case Description
                    set toggl_desc $kv[2]
                case Duration
                    set toggl_duration $kv[2]
            end
        end

        echo $toggl_desc $toggl_duration
    end

    set_color normal
end
