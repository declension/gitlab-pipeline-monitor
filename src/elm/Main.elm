module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key, replaceUrl)
import Dict
import Model exposing (Flags, Host, Model, Msg(..), Pipeline, Project, Status(..), Token)
import Result exposing (Result)
import Time
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
            { config = flags, key = key, token = token, url = url, data = { pipelines = Dict.empty, projects = [] } }
    in
    ( model, maybeGetRootData key url flags.gitlabHost token )


maybeGetRootData : Key -> Url -> Host -> Maybe Token -> Cmd Msg
maybeGetRootData key siteUrl host maybeToken =
    case maybeToken of
        Nothing ->
            Debug.log "No token found!"
            Cmd.none

        Just token ->
            let
                newUrl =
                    { siteUrl | path = "/", fragment = Nothing, query = Nothing }
            in
            Cmd.batch
                [ replaceUrl key (Url.toString newUrl)
                , getUrl token (projectsUrl host) GotProjects projectsDecoder
                ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        data =
            model.data
    in
    case msg of
        Tick t ->
            case model.token of
                Just token ->
                    ( model, getUrl token (projectsUrl model.config.gitlabHost) GotProjects projectsDecoder )

                _ ->
                    ( model, Cmd.none )

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

        GotPipelinesFor projectId result ->
            case result of
                Err err ->
                    Debug.log ("FAILED to get pipelines (" ++ Debug.toString err ++ "). Current model")
                        ( model, Cmd.none )

                Ok values ->
                    let
                        newPipelines =
                            Dict.insert projectId values data.pipelines
                    in
                    Debug.log ("Got pipelines" ++ Debug.toString values)
                        ( { model | data = { data | pipelines = newPipelines } }, Cmd.none )

        GotProjects result ->
            case result of
                Ok projects ->
                    let
                        cmds =
                            projects
                                |> List.map (cmdForProject model)
                                |> Cmd.batch
                    in
                    Debug.log ("Got projects " ++ Debug.toString projects)
                        ( { model | data = { data | projects = projects } }, cmds )

                Err err ->
                    Debug.log ("FAILED to get projects (" ++ Debug.toString err ++ "). Current model")
                        ( model, Cmd.none )


cmdForProject : Model -> Project -> Cmd Msg
cmdForProject model project =
    case model.token of
        Just token ->
            getUrl token (pipelinesUrl model.config.gitlabHost project.id) (GotPipelinesFor project.id) pipelinesDecoder

        _ ->
            Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (10 * 1000) Tick
