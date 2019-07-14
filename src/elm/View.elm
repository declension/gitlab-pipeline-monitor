module View exposing (..)
import Browser
import Html exposing (Html, a, h3, li, main_, nav, ol, span, text, ul)
import Html.Attributes exposing (class, href, target)
import Model exposing (Flags, Model, Msg, Pipeline, Status(..))
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