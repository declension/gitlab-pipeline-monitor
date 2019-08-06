module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key, replaceUrl)
import Dict exposing (Dict)
import Model exposing (Flags, GitRef, Host, Model, Msg(..), Pipeline, PipelineId, Project, ProjectId, Status(..), Token)
import Result exposing (Result)
import Time
import Url exposing (Protocol(..), Url)
import Utils exposing (ifNothing)
import View exposing (view)
import Wire exposing (extractToken, getUrl, pipelineDetailDecoder, pipelineDetailUrl, pipelinesDecoder, pipelinesUrl, projectsDecoder, projectsUrl)


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
        FetchProjects _ ->
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

                Ok pipelines ->
                    let
                        newPipelines =
                            Dict.update projectId (modifyProject pipelines) data.pipelines

                        newModel =
                            { model | data = { data | pipelines = newPipelines } }

                        cmds =
                            pipelines |> List.map (cmdForPipelineDetail newModel projectId) |> Cmd.batch
                    in
                    ( newModel, cmds )

        GotProjects result ->
            case result of
                Ok projects ->
                    let
                        cmds =
                            projects
                                |> List.map (.id >> cmdForProject model)
                                |> Cmd.batch
                    in
                    ( { model | data = { data | projects = Debug.log "Got projects " projects } }, cmds )

                Err err ->
                    Debug.log ("FAILED to get projects (" ++ Debug.toString err ++ "). Current model")
                        ( model, Cmd.none )

        GotPipelineDetailFor projectId gitRef result ->
            case result of
                Ok pipelineDetail ->
                    let
                        updateDetail =
                            Maybe.map (\pipeline -> { pipeline | detail = Just pipelineDetail })

                        updateGroup =
                            Maybe.map (Dict.update pipelineDetail.id updateDetail)

                        updateProject =
                            Maybe.map (Dict.update gitRef updateGroup)

                        newPipelines =
                            model.data.pipelines
                                |> Dict.update projectId updateProject
                    in
                    ( { model | data = { data | pipelines = newPipelines } }, Cmd.none )

                Err err ->
                    Debug.log ("FAILED getting pipeline detail:" ++ Debug.toString err) ( model, Cmd.none )


modifyProject : List Pipeline -> Maybe (Dict GitRef (Dict PipelineId Pipeline)) -> Maybe (Dict GitRef (Dict PipelineId Pipeline))
modifyProject pipelines existing =
    case existing of
        Just d ->
            updatePipelines d pipelines |> Just

        Nothing ->
            updatePipelines Dict.empty pipelines |> Just


const : Pipeline -> Maybe Pipeline -> Maybe Pipeline
const pipeline existing =
    case existing of
        Just old ->
            case old.detail of
                -- Don't update if we have detail
                Just _ ->
                    existing

                Nothing ->
                    Just pipeline

        Nothing ->
            Just pipeline


updateGroupFor : Pipeline -> Maybe (Dict PipelineId Pipeline) -> Maybe (Dict PipelineId Pipeline)
updateGroupFor pipeline existing =
    case existing of
        Just old ->
            Dict.update pipeline.id (const pipeline) old |> Just

        Nothing ->
            Dict.fromList [ ( pipeline.id, pipeline ) ] |> Just


updatePipelines : Dict GitRef (Dict PipelineId Pipeline) -> List Pipeline -> Dict GitRef (Dict PipelineId Pipeline)
updatePipelines projectPipelines pipelines =
    List.foldl (\p d -> Dict.update p.ref (updateGroupFor p) d) projectPipelines pipelines


cmdForProject : Model -> ProjectId -> Cmd Msg
cmdForProject model projectId =
    case model.token of
        Just token ->
            getUrl token (pipelinesUrl model.config.gitlabHost projectId) (GotPipelinesFor projectId) pipelinesDecoder

        _ ->
            Cmd.none


cmdForPipelineDetail : Model -> ProjectId -> Pipeline -> Cmd Msg
cmdForPipelineDetail model projectId pipeline =
    let
        url =
            pipelineDetailUrl model.config.gitlabHost projectId pipeline.id

        msg =
            GotPipelineDetailFor projectId pipeline.ref

        getIt token =
            getUrl token url msg pipelineDetailDecoder

        currentPipeline =
            Dict.get projectId model.data.pipelines
                |> Maybe.andThen (Dict.get pipeline.ref)
                |> Maybe.andThen (Dict.get pipeline.id)
    in
    case model.token of
        Just token ->
            case currentPipeline of
                Just cur ->
                    case cur.detail of
                        Just _ ->
                            -- Got some detail, but maybe we don't need to update it
                            case cur.status of
                                Failed ->
                                    Cmd.none

                                Success ->
                                    Cmd.none

                                Cancelled ->
                                    Cmd.none

                                _ ->
                                    Debug.log "Getting live one!"
                                        getIt
                                        token

                        Nothing ->
                            getIt token

                Nothing ->
                    Debug.log ("Have no pipeline somehow for " ++ pipeline.url)
                        Cmd.none

        _ ->
            Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (20 * 1000) FetchProjects
