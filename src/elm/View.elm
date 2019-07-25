module View exposing (iconFor, maybeViewOauthLink, pipelineItemOf, view, viewLink)

import Browser
import Config exposing (maxBuildsPerBranch, maxNonDefaultBranches)
import Dict exposing (Dict)
import Html exposing (Html, a, div, h2, li, main_, nav, ol, small, span, text, ul)
import Html.Attributes exposing (class, classList, href, id, target)
import Model exposing (Flags, GitRef, Model, Msg, Pipeline, PipelineStore, Project, ProjectId, Status(..))
import Url exposing (Protocol(..), Url)
import Utils exposing (prepend, relativise)
import Wire exposing (authUrlFor)



-- VIEW


view : Model -> Browser.Document msg
view model =
    { title = "Gitlab Pipelines status"
    , body =
        [ main_ [ class ("pg-" ++ model.url.path) ] [ div [ id "wrapper", classList [ ( "loading", List.isEmpty model.data.projects ) ] ] (viewMain model) ]
        , nav []
            [ ul []
                [ viewLink "/" "all projects"
                ]
            , div [] [ text "© 2019. ", a [ href "https://github.com/declension/" ] [ text "On GitHub" ] ]
            ]
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
    List.map (viewProjectFromPipelinesData model.data.pipelines) model.data.projects


viewProjectFromPipelinesData : Dict ProjectId (List Pipeline) -> Project -> Html msg
viewProjectFromPipelinesData allPipelines project =
    let
        pipelinesByGitRef =
            Dict.get project.id allPipelines |> Maybe.map byGitRef |> Maybe.withDefault Dict.empty
    in
    viewProject pipelinesByGitRef project


byGitRef : List Pipeline -> Dict GitRef (List Pipeline)
byGitRef pipelines =
    pipelines |> List.map (\p -> ( p.ref, p )) |> List.foldl addItem Dict.empty


addItem : ( GitRef, Pipeline ) -> Dict GitRef (List Pipeline) -> Dict GitRef (List Pipeline)
addItem ( gitRef, pipeline ) cur =
    Dict.update gitRef (appendItem pipeline) cur


appendItem : a -> Maybe (List a) -> Maybe (List a)
appendItem item maybeExistingList =
    maybeExistingList |> Maybe.map (\existing -> Just (item :: existing)) |> Maybe.withDefault (Just [ item ])


viewProject : Dict GitRef (List Pipeline) -> Project -> Html msg
viewProject pipelines project =
    div [ class "project" ]
        ([ a [] [ text project.namespace ]
         , a [ href project.url, target "_blank" ]
            [ h2 [] [ text project.name ]
            ]
         ]
            ++ maybeDescription project.description
            ++ [ viewProjectPipelines pipelines ]
        )


maybeDescription : Maybe String -> List (Html msg)
maybeDescription maybeDesc =
    maybeDesc
        |> Maybe.map (\str -> [ div [ class "description" ] [ text str ] ])
        |> Maybe.withDefault []


viewProjectPipelines : Dict GitRef (List Pipeline) -> Html msg
viewProjectPipelines pipelineGroups =
    if Dict.isEmpty pipelineGroups then
        div [ class "empty" ] [ text "😴" ]

    else
        let
            masterGroup =
                Dict.get "master" pipelineGroups |> Maybe.map (List.take maxBuildsPerBranch) |> Maybe.withDefault []

            others =
                Dict.remove "master" pipelineGroups
        in
        ol [ class "pipeline-groups" ]
            (viewPipelineGroup ( "master", masterGroup )
                :: (others |> Dict.toList |> List.sortWith latestIdComparer |> List.take maxNonDefaultBranches |> List.map viewPipelineGroup)
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


viewPipelineGroup : ( GitRef, List Pipeline ) -> Html msg
viewPipelineGroup ( gitRef, pipelines ) =
    li [ classList [ ( "master", gitRef == "master" ), ( "group", True ) ] ]
        [ a [ href "#", target "_blank" ]
            [ text gitRef ]
        , div [ class "pipelines" ]
            (pipelines |> List.reverse |> List.take maxBuildsPerBranch |> List.map pipelineButtonOf)
        ]


pipelineButtonOf : Pipeline -> Html msg
pipelineButtonOf content =
    a [ class "pipeline", href content.url, target "_blank", class <| classFor content.status ]
        [ span [ class "emoji" ] [ text <| iconFor content.status ], span [ class "small" ] [ content.id |> String.fromInt |> text ] ]


pipelineItemOf : Pipeline -> Html msg
pipelineItemOf content =
    li []
        [ a [ href content.url, target "_blank", class <| classFor content.status ]
            [ small [] [ text <| "#" ++ String.fromInt content.id ++ " " ]
            , text content.ref
            , span [ class "emoji" ] [ iconFor content.status |> prepend " " |> text ]
            ]
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



--            "😶"


viewLink : String -> String -> Html msg
viewLink path name =
    li [] [ a [ href path ] [ text name ] ]
