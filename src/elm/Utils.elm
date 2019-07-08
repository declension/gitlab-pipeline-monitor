module Utils exposing (prepend, ifNothing)


prepend : String -> String -> String
prepend prefix str =
    prefix ++ str


ifNothing : Maybe a -> Maybe a -> Maybe a
ifNothing default val =
    case val of
        Nothing ->
            default

        Just x ->
            Just x
