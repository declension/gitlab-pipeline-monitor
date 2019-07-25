module View exposing (iconFor, maybeViewOauthLink, pipelineItemOf, view, viewLink)

import Browser
import Config exposing (maxBuildsPerBranch, maxNonDefaultBranches)
import Dict exposing (Dict)
import Html exposing (Html, a, b, div, h3, li, main_, nav, ol, small, span, text, ul)
import Html.Attributes exposing (class, classList, href, target)
import Model exposing (Flags, GitRef, Model, Msg, Pipeline, PipelineStore, Project, ProjectId, Status(..))
import Url exposing (Protocol(..), Url)
import Utils exposing (prepend, relativise)
import Wire exposing (authUrlFor)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Gitlab Pipelines status"
    , body =
        [ main_ [ class <| "pg-" ++ model.url.path ]
            (List.map (viewProjectFromPipelinesData model.data.pipelines) model.data.projects
                ++ maybeViewOauthLink model
            )
        , nav []
            [ ul []
                [ viewLink "/" "all projects"
                ],
                div [] [text "© 2019. ", a [href "https://github.com/declension/"] [text "On GitHub"]]
            ]
        ]
    }


viewProjectFromPipelinesData : Dict ProjectId (List Pipeline) -> Project -> Html Msg
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


viewProject : Dict GitRef (List Pipeline) -> Project -> Html Msg
viewProject pipelines project =
    div [ class "project" ]
        ([ a [] [ text project.namespace ]
         , a [ href project.url, target "_blank" ]
            [ h3 [] [ text project.name ]
            ]
         ]
            ++ maybeDescription project.description
            ++ [ viewProjectPipelines pipelines ]
        )


maybeDescription : Maybe String -> List (Html Msg)
maybeDescription maybeDesc =
    maybeDesc
        |> Maybe.map (\str -> [ div [ class "description" ] [ text str ] ])
        |> Maybe.withDefault []


viewProjectPipelines : Dict GitRef (List Pipeline) -> Html Msg
viewProjectPipelines pipelineGroups =
    if Dict.isEmpty pipelineGroups then
        div [ class "empty" ] [ text "😴" ]

    else
        let
            masterGroup =
                -- TODO: work out what's reversing this list...
                Dict.get "master" pipelineGroups |> Maybe.map List.reverse |> Maybe.map (List.take maxBuildsPerBranch) |> Maybe.withDefault []
            others = Dict.remove "master" pipelineGroups
        in
            div [ class "pipeline-groups" ]
                (viewPipelineGroup ("master", masterGroup) ::
                (others |> Dict.toList |> List.take maxNonDefaultBranches |> List.map viewPipelineGroup))


maybeViewOauthLink : Model -> List (Html msg)
maybeViewOauthLink model =
    case model.token of
        Nothing ->
            [ b [] [ a [ href <| Url.toString <| authUrlFor model.config (relativise model.url "/redirect") ] [ text "Authorise in GitLab" ] ] ]

        _ ->
            []


viewPipelineGroup : ( GitRef, List Pipeline ) -> Html msg
viewPipelineGroup ( gitRef, pipelines ) =
    ol []
        [ li [ classList [ ( "master", gitRef == "master" ), ( "group", True ) ] ]
            [ a [ href "#", target "_blank" ]
                [ text gitRef ]
            , div [ class "pipelines" ]
                (pipelines |> List.reverse |> List.take maxBuildsPerBranch |> List.map pipelineButtonOf)
            ]
        ]


pipelineButtonOf : Pipeline -> Html msg
pipelineButtonOf content =
    a [ class "pipeline", href content.url, target "_blank" , class <| classFor content.status ]
        [ span [ class "emoji" ] [ text <| iconFor content.status ], small [] [ content.id |> String.fromInt |> text ] ]


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
            " "

        Cancelled ->
            "\u{1F92F}"

        _ ->
            "😶"


viewLink : String -> String -> Html msg
viewLink path name =
    li [] [ a [ href path ] [ text name ] ]
