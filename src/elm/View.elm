module View exposing (iconFor, maybeViewOauthLink, pipelineItemOf, view, viewLink)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, b, div, h3, li, main_, nav, ol, small, span, text, ul)
import Html.Attributes exposing (class, href, target)
import Model exposing (Flags, Model, Msg, Pipeline, Project, ProjectId, Status(..))
import Url exposing (Protocol(..), Url)
import Utils exposing (prepend, relativise)
import Wire exposing (authUrlFor)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Gilab Pipelines status"
    , body =
        [ main_ [ class <| "pg-" ++ model.url.path ]
            (List.map (viewProjectFromPipelinesData model.data.pipelines) model.data.projects
                ++ maybeViewOauthLink model
            )
        , nav []
            [ ul []
                [ viewLink "/" "all"
                ]
            ]
        ]
    }


viewProjectFromPipelinesData : Dict ProjectId (List Pipeline) -> Project -> Html Msg
viewProjectFromPipelinesData allPipelines project =
    viewProject (Dict.get project.id allPipelines |> Maybe.withDefault []) project


viewProject : List Pipeline -> Project -> Html Msg
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


viewProjectPipelines : List Pipeline -> Html Msg
viewProjectPipelines pipelines =
    ol [ class "pipelines" ] <| List.map pipelineItemOf pipelines


maybeViewOauthLink : Model -> List (Html msg)
maybeViewOauthLink model =
    case model.token of
        Nothing ->
            [ b [] [ a [ href <| Url.toString <| authUrlFor model.config (relativise model.url "/redirect") ] [ text "Authorise in GitLab" ] ] ]

        _ ->
            []


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

        _ ->
            ""


iconFor status =
    case status of
        Success ->
            "😀"

        Failed ->
            "😟"

        _ ->
            "😶"


viewLink : String -> String -> Html msg
viewLink path name =
    li [] [ a [ href path ] [ text name ] ]
