module View exposing (iconFor, maybeViewOauthLink, view)

import Browser
import Config exposing (maxBuildsPerBranch, maxNonDefaultBranches)
import DateFormat.Relative exposing (relativeTime)
import Dict exposing (Dict)
import Html exposing (Html, a, div, footer, h2, h3, img, li, main_, ol, small, span, text)
import Html.Attributes exposing (class, classList, href, id, src, target)
import Model exposing (Flags, GitRef, Model, Msg, Pipeline, PipelineDetail, PipelineStore, Project, ProjectId, Status(..))
import Time exposing (Posix)
import Url exposing (Protocol(..), Url)
import Utils exposing (relativise)
import Wire exposing (authUrlFor)


view : Model -> Browser.Document msg
view model =
    { title = "Gitlab Pipelines status"
    , body =
        [ main_ [ class ("pg-" ++ model.url.path) ] [ div [ id "wrapper", classList [ ( "loading", List.isEmpty model.data.projects ) ] ] (viewMain model) ]
        , nav []
            [ ul []
                [ viewLink "/" "all projects"
                ]
            ]
        , footer [] [ div [] [ text "© 2019. ", a [ href "https://github.com/declension/" ] [ text "On GitHub" ] ] ]
        ]
    }


viewMain : Model -> List (Html msg)
viewMain model =
    case model.token of
        Nothing ->
            maybeViewOauthLink model

        Just _ ->
            if List.isEmpty model.data.projects then
                [ div [ id "full-screen" ] [ h2 [] [ text "Loading" ] ] ]

            else
                viewProjects model


viewProjects : Model -> List (Html msg)
viewProjects model =
    List.map (viewProjectFromPipelinesData model.now model.data.pipelines) model.data.projects


viewProjectFromPipelinesData : Posix -> PipelineStore -> Project -> Html msg
viewProjectFromPipelinesData now allPipelines project =
    let
        innerValues _ =
            Dict.values

        pipelinesByGitRef =
            Dict.get project.id allPipelines
                |> Maybe.map (Dict.map innerValues)
                |> Maybe.withDefault Dict.empty
    in
    viewProject now pipelinesByGitRef project


viewProject : Posix -> Dict GitRef (List Pipeline) -> Project -> Html msg
viewProject now pipelines project =
    div [ class "project" ]
        ([ a [] [ text project.namespace ]
         , a [ href project.url, target "_blank" ]
            [ h2 [] [ text project.name ]
            ]
         ]
            ++ maybeDescription project.description
            ++ [ viewProjectPipelines now pipelines ]
            ++ [small [] [text <| "Last updated " ++  relativeTime now project.lastActivity]]
        )


maybeDescription : Maybe String -> List (Html msg)
maybeDescription maybeDesc =
    maybeDesc
        |> Maybe.map (\str -> [ div [ class "description" ] [ text str ] ])
        |> Maybe.withDefault []


viewProjectPipelines : Posix -> Dict GitRef (List Pipeline) -> Html msg
viewProjectPipelines now pipelineGroups =
    if Dict.isEmpty pipelineGroups then
        div [ class "empty" ] [ text "😴" ]

    else
        let
            masterGroup =
                Dict.get "master" pipelineGroups |> Maybe.withDefault []

            others =
                Dict.remove "master" pipelineGroups
        in
        ol [ class "pipeline-groups" ]
            (viewPipelineGroup now ( "master", masterGroup )
                :: (others |> Dict.toList |> List.sortWith latestIdComparer |> List.take maxNonDefaultBranches |> List.map (viewPipelineGroup now))
            )


latestIdComparer : ( a, List { b | id : Int } ) -> ( a, List { b | id : Int } ) -> Order
latestIdComparer ( _, leftBuilds ) ( _, rightBuilds ) =
    let
        getLatestId =
            List.map .id >> List.maximum >> Maybe.withDefault 0
    in
    compare (getLatestId rightBuilds) (getLatestId leftBuilds)


maybeViewOauthLink : Model -> List (Html msg)
maybeViewOauthLink model =
    case model.token of
        Nothing ->
            [ div [ id "full-screen" ] [ a [ href <| Url.toString <| authUrlFor model.config (relativise model.url "/redirect") ] [ text "Authorise in GitLab" ] ] ]

        _ ->
            []


viewPipelineGroup : Posix -> ( GitRef, List Pipeline ) -> Html msg
viewPipelineGroup now ( gitRef, pipelines ) =
    li [ classList [ ( "master", gitRef == "master" ), ( "group", True ) ] ]
        [ h3 []
            [ a [ href "#", target "_blank" ]
                [ text gitRef ]
            ]
        , div [ class "pipelines" ]
            (pipelines |> List.reverse |> List.take maxBuildsPerBranch |> List.map (pipelineButtonOf now))
        ]


pipelineButtonOf : Posix -> Pipeline -> Html msg
pipelineButtonOf now content =
    a [ class "pipeline", href content.url, target "_blank", class <| classFor content.status ]
        ([ span [ class "emoji" ] [ text <| iconFor content.status ]
         , span [ class "small" ] [ content.id |> String.fromInt |> text ]
         ]
            ++ (Maybe.map (detail now) content.detail |> Maybe.withDefault [])
        )


detail : Posix -> PipelineDetail -> List (Html msg)
detail now d =
    [ img [ class "avatar", src (Maybe.withDefault "#" d.user.avatarUrl) ] [ text d.user.name ]
    , small  [] [ text <| relativeTime now d.createdAt ]
    ]


classFor status =
    case status of
        Success ->
            "success"

        Failed ->
            "failed"

        Running ->
            "running"

        _ ->
            ""


iconFor status =
    case status of
        Success ->
            "😀"

        Failed ->
            "😟"

        Running ->
            "⏳"

        Cancelled ->
            "\u{1F92F}"

        Pending ->
            "😴"
