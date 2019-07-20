module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key, replaceUrl)
import Model exposing (Flags, Model, Msg(..), Pipeline, Status(..), Token)
import Result exposing (Result)
import Url exposing (Protocol(..), Url)
import Utils exposing (ifNothing)
import View exposing (view)
import Wire exposing (extractToken, getUrl, pipelinesDecoder, pipelinesUrl, projectsDecoder, projectsUrl)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        token =
            extractToken url

        model =
            { config = flags, key = key, token = token, url = url, data = { pipelines = [], projects = [] } }
    in
    ( model, maybeGetRootData key url flags token )


maybeGetRootData : Key -> Url -> Flags -> Maybe Token -> Cmd Msg
maybeGetRootData key siteUrl flags maybeToken =
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
                , getUrl token (pipelinesUrl flags) GotPipelines pipelinesDecoder
                , getUrl token (projectsUrl flags) GotProjects projectsDecoder
                ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            model.data
    in
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

        GotPipelines result ->
            case result of
                Err err ->
                    Debug.log ("FAILED to get pipelines (" ++ Debug.toString err ++ "). Current model")
                        ( model, Cmd.none )

                Ok values ->
                    Debug.log ("Got pipelines" ++ Debug.toString values)
                        ( { model | data = { data | pipelines = values } }, Cmd.none )

        GotProjects result ->
            case result of
                Ok projects ->
                    Debug.log ("Got projects " ++ Debug.toString projects)
                        ( { model | data = { data | projects = projects } }, Cmd.none )

                Err err ->
                    Debug.log ("FAILED to get projects (" ++ Debug.toString err ++ "). Current model")
                        ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
