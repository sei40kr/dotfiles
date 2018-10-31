function balias --argument-names name def
    if not functions -q $name
        alias $name $def
        funcsave $name
    end
end
