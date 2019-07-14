module Utils exposing (ifNothing, prepend, relativise, stripQuestion)

import Url exposing (Url)


{-| Like String.append, but adds to the front. Useful when you don't have `flip`...
-}
prepend : String -> String -> String
prepend prefix str =
    prefix ++ str


stripQuestion =
    String.dropLeft 1


{-| Provide a default if `Nothing`, but stay in the Maybe monad.
-}
ifNothing : Maybe a -> Maybe a -> Maybe a
ifNothing default val =
    case val of
        Nothing ->
            default

        Just x ->
            Just x


{-| Produce a new URL given a new path (relative or absolute)
-}
relativise : Url -> String -> Url
relativise base new =
    case String.uncons new of
        Just ( '/', _ ) ->
            { base | path = new }

        Just ( _, _ ) ->
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
