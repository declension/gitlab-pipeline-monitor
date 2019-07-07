-- Main.elm


module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, li, main_, p, pre, text, ul)
import Html.Attributes exposing (class, href)
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ pre [] [ text (Url.toString model.url) ]
        , p [] [ text <| "Note that token: " ++ (Maybe.withDefault "(none)" <| Maybe.andThen Url.percentDecode <| Maybe.andThen .query <| Maybe.andThen Url.fromString <| Maybe.map (\x -> "https://X'?" ++ x) model.url.fragment) ]
        , ul []
            [ viewLink "/about"
            , viewLink "/home"
            ]
        , main_ [ class <| "pg-" ++ model.url.path ]
            [ p [] [ text """Lorem ipsum dolor sit amet consectetur adipiscing elit gravida et,
                             fusce montes magnis aptent sagittis convallis praesent molestie dictumst turpis,
                             porttitor mauris pretium quis faucibus morbi himenaeos tortor.
                             Semper class nullam metus pretium habitant tellus inceptos venenatis facilisi laoreet
                             etiam massa, cum eu dui fusce lobortis sem magna duis iaculis aliquam.
                             A dui eleifend condimentum felis aliquam mus nulla,
                             quam primis vehicula nec varius justo imperdiet,
                             habitant fames hac vel cursus egestas.""" ]
            ]
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
