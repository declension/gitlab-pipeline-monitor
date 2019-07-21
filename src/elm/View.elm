module View exposing (authUrlFor, iconFor, maybeViewOauthLink, pipelineItemOf, toQueryPair, view, viewLink)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, b, div, h3, li, main_, nav, ol, small, span, text, ul)
import Html.Attributes exposing (class, href, target)
import Model exposing (Flags, Model, Msg, Pipeline, Project, ProjectId, Status(..))
import Url exposing (Protocol(..), Url)
import Url.Builder as Builder
import Utils exposing (prepend, relativise)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "ARRIVAL RED: CI status"
    , body =
        [ nav []
            [ ul []
                [ viewLink "/" "all"
                ]
            ]
        , main_ [ class <| "pg-" ++ model.url.path ]
            (List.map (viewProjectFromPipelinesData model.data.pipelines) model.data.projects
                ++ maybeViewOauthLink model
            )
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
        |> Maybe.map (\str -> [ small [] [ text str ] ])
        |> Maybe.withDefault []


viewProjectPipelines : List Pipeline -> Html Msg
viewProjectPipelines pipelines =
    ol [ class "pipelines" ] <| List.map pipelineItemOf pipelines


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
