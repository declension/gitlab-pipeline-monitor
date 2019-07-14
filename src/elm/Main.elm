module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key, replaceUrl)
import Http exposing (Error, Header, emptyBody, expectJson, header)
import Json.Decode as D exposing (Decoder)
import Model exposing (Flags, Model, Msg(..), Pipeline, Status(..), Token)
import Result exposing (Result)
import Url exposing (Protocol(..), Url)
import Url.Builder as Builder exposing (toQuery)
import Url.Parser exposing (parse, query)
import Url.Parser.Query as Query
import Utils exposing (ifNothing, prepend, stripQuestion)
import View exposing (view)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


emptyHttps : Url
emptyHttps =
    { protocol = Https, host = "", port_ = Nothing, path = "", query = Nothing, fragment = Nothing }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        token =
            extractToken url

        fullUrl =
            { emptyHttps
                | host = flags.gitlabHost
                , path = "/api/v4/projects/" ++ String.fromInt flags.gitlabProject ++ "/pipelines"
                , query = Just <| stripQuestion <| toQuery [ Builder.int "per_page" 50 ]
            }
    in
    ( { config = flags, key = key, token = token, url = url, pipelines = [] }, maybeGetRootData key url fullUrl token )


maybeGetRootData : Key -> Url -> Url -> Maybe Token -> Cmd Msg
maybeGetRootData key siteUrl endpointUrl maybeToken =
    case maybeToken of
        Nothing ->
            Cmd.none

        Just token ->
            let
                newUrl =
                    { siteUrl | path = "/", fragment = Nothing, query = Nothing }
            in
            Cmd.batch
                [ replaceUrl key (Url.toString newUrl)
                , Http.request
                    { method = "GET"
                    , url = Url.toString endpointUrl
                    , body = emptyBody
                    , expect = expectJson GotProjects pipelinesDecoder
                    , headers = [ header "Authorization" ("Bearer " ++ token) ]
                    , timeout = Just (10.0 * 1000.0)
                    , tracker = Nothing
                    }
                ]


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
