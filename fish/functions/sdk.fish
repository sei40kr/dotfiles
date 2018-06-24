# sdk.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

function sdk
    if not type -q sdk-wrapper
        echo 'ERROR: sdk-wrapper is not found.' >&2
        return 1
    end

    sdk-wrapper $argv
end
