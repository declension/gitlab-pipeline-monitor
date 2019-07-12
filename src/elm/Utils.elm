module Utils exposing (ifNothing, prepend, relativise)

import Url exposing (Url)


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



--| Produce a relative URL given a new path
relativise : Url -> String -> Url
relativise base new =
    case String.uncons new of
        Just ( '/', _ ) ->
            { base | path = new }

        Just ( _, rest ) ->
            let
                suffix =
                    if String.endsWith "/" base.path then
                        ""

                    else
                        "/"
            in
            { base | path = base.path ++ suffix ++ new }

        _ ->
            base
