-- Main.elm


module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, h3, li, main_, nav, ol, span, text, ul)
import Html.Attributes exposing (class, href, target)
import Http exposing (Error, Header, emptyBody, expectJson, header)
import Json.Decode as D exposing (Decoder)
import List
import Result exposing (Result)
import Url exposing (Protocol(..), Url)
import Url.Builder as Builder exposing (QueryParameter(..))
import Url.Parser exposing (parse, query)
import Url.Parser.Query as Query
import Utils exposing (ifNothing, prepend, relativise)


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
    { config : Flags
    , key : Nav.Key
    , token : Maybe Token
    , pipelines : List Pipeline
    , url : Url.Url
    }


type alias Flags =
    { gitlabHost : String
    , gitlabProject : Int
    , gitlabAppId : String
    }


type Status
    = Success
    | Failed
    | Pending
    | Cancelled
    | Unknown


type alias Pipeline =
    { ref : String
    , id : Int
    , status : Status
    , url : String
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        token =
            extractToken url

        -- TODO: use proper URLs
        fullUrl =
            "https://" ++ flags.gitlabHost ++ "/api/v4/projects/" ++ String.fromInt flags.gitlabProject ++ "/pipelines?per_page=50"
    in
    ( { config = flags, key = key, token = token, url = url, pipelines = [] }, maybeGetRootData fullUrl token )


maybeGetRootData : String -> Maybe Token -> Cmd Msg
maybeGetRootData fullUrl maybeToken =
    case maybeToken of
        Nothing ->
            Debug.log "Not doing HTTP, no token"
                Cmd.none

        Just token ->
            Http.request
                { method = "GET"
                , url = fullUrl
                , body = emptyBody
                , expect = expectJson GotProjects pipelinesDecoder
                , headers = [ header "Authorization" ("Bearer " ++ token) ]
                , timeout = Just (10.0 * 1000.0)
                , tracker = Nothing
                }


pipelinesDecoder : Decoder (List Pipeline)
pipelinesDecoder =
    D.list pipelineDecoder


pipelineDecoder : Decoder Pipeline
pipelineDecoder =
    D.map4 Pipeline
        (D.field "ref" D.string)
        (D.field "id" D.int)
        (D.field "status" statusDecoder)
        (D.field "web_url" D.string)


statusDecoder : Decoder Status
statusDecoder =
    let
        conv val =
            case val of
                "success" ->
                    Success

                "failed" ->
                    Failed

                _ ->
                    Unknown
    in
    D.string |> D.map conv


projectsDecoder : Decoder (List String)
projectsDecoder =
    D.list (D.field "name" D.string)


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
    | GotProjects (Result Error (List Pipeline))
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

        GotProjects (Err err) ->
            Debug.log ("Massive fail (" ++ Debug.toString err ++ "). Current model")
                ( model, Cmd.none )

        GotProjects (Ok values) ->
            Debug.log ("GOT something " ++ Debug.toString values)
                ( { model | pipelines = values }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "ARRIVAL RED: CI status"
    , body =
        [ nav []
            [ ul []
                [ viewLink "/core"
                , viewLink "/research"
                ]
            ]
        , main_ [ class <| "pg-" ++ model.url.path ]
            ([ ol [ class "pipelines" ] (List.map pipelineItemOf model.pipelines)
             ]
                ++ maybeViewOauthLink model
            )
        ]
    }


authUrlFor : Flags -> Url -> Url
authUrlFor config currentUrl =
    { protocol = Https
    , host = config.gitlabHost
    , port_ = Nothing
    , path = Builder.absolute [ "oauth", "authorize" ] []
    , query =
        Just <|
            String.join "&" <|
                List.map toQueryPair
                    [ ( "client_id", config.gitlabAppId )
                    , ( "response_type", "token" )

                    -- TODO: inject state and persist
                    , ( "state", "1234" )
                    , ( "redirect_uri", Url.toString currentUrl )
                    ]
    , fragment = Nothing
    }


toQueryPair : ( String, String ) -> String
toQueryPair ( key, value ) =
    Url.percentEncode key ++ "=" ++ Url.percentEncode value


maybeViewOauthLink : Model -> List (Html msg)
maybeViewOauthLink model =
    case model.token of
        Nothing ->
            [ h3 [] [ a [ href <| Url.toString <| authUrlFor model.config (relativise model.url "/redirect") ] [ text "Authorise in GitLab" ] ] ]

        _ ->
            []


pipelineItemOf : Pipeline -> Html msg
pipelineItemOf content =
    li []
        [ a [ href content.url, target "_blank" ]
            [ text content.ref
            , span [ class "emoji" ] [ iconFor content.status |> prepend " " |> text ]
            ]
        ]


iconFor status =
    case status of
        Success ->
            "😀"

        Failed ->
            "😟"

        _ ->
            "😶"


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
