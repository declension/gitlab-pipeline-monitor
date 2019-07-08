-- Main.elm


module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, li, main_, p, pre, text, ul)
import Html.Attributes exposing (class, href)
import Url exposing (Url)
import Url.Parser exposing (parse, query)
import Url.Parser.Query as Query
import Utils exposing (ifNothing, prepend)


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


type alias Token =
    String


type alias Model =
    { key : Nav.Key
    , token : Maybe Token
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key (extractToken url) url, Cmd.none )


extractToken : Url -> Maybe Token
extractToken url =
    url.fragment
        |> Maybe.map (prepend "http://DUMMY?")
        |> Maybe.andThen Url.fromString
        |> Maybe.andThen (parse (query toToken))
        |> Maybe.withDefault Nothing


toToken : Query.Parser (Maybe Token)
toToken =
    Query.string "access_token"



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
                    let
                        newModel =
                            { model | token = ifNothing (extractToken url) model.token }
                    in
                    Debug.log "Internal" ( newModel, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    Debug.log "External"
                        ( model, Nav.load href )

        UrlChanged url ->
            let
                newModel =
                    { model | url = url, token = ifNothing (extractToken url) model.token }
            in
            Debug.log "UrlChanged"
                ( newModel, Cmd.none )



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
        , ul []
            [ viewLink "/about"
            , viewLink "/home"
            ]
        , main_ [ class <| "pg-" ++ model.url.path ]
            [ pre [] [ text (Maybe.withDefault "(none)" model.token) ]
            , p [] [ text """Lorem ipsum dolor sit amet consectetur adipiscing elit gravida et,
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
