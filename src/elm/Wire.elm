module Wire exposing (authUrlFor, blankable, emptyHttps, extractToken, getUrl, pipelineDecoder, pipelinesDecoder, pipelinesUrl, projectDecoder, projectsDecoder, projectsUrl, statusDecoder, toQueryPair, toToken)

import Http exposing (emptyBody, expectJson, header)
import Iso8601
import Json.Decode as D exposing (Decoder)
import Model exposing (Flags, Msg(..), Pipeline, Project, ProjectId, Status(..), Token)
import Url exposing (Protocol(..), Url)
import Url.Builder as Builder exposing (toQuery)
import Url.Parser exposing (parse, query)
import Url.Parser.Query as Query
import Utils exposing (prepend, stripQuestion)


getUrl : Token -> Url -> (Result Http.Error a -> Msg) -> Decoder a -> Cmd Msg
getUrl token url msg decoder =
    Http.request
        { method = "GET"
        , url = Url.toString url
        , body = emptyBody
        , expect = expectJson msg decoder
        , headers = [ header "Authorization" ("Bearer " ++ token) ]
        , timeout = Just (10.0 * 1000.0)
        , tracker = Nothing
        }


pipelinesUrl : String -> ProjectId -> Url
pipelinesUrl host projectId =
    { emptyHttps
        | host = host
        , path = "/api/v4/projects/" ++ String.fromInt projectId ++ "/pipelines"
        , query = [ Builder.int "per_page" 6 ] |> toQuery |> stripQuestion |> Just
    }


projectsUrl : String -> Url
projectsUrl host =
    { emptyHttps
        | host = host
        , path = "/api/v4/projects/"
        , query =
            [ Builder.int "per_page" 40, Builder.int "archived" 0, Builder.int "membership" 1, Builder.string "order_by" "last_activity_at" ]
                |> toQuery
                |> stripQuestion
                |> Just
    }


emptyHttps : Url
emptyHttps =
    { protocol = Https, host = "", port_ = Nothing, path = "", query = Nothing, fragment = Nothing }


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


projectsDecoder : Decoder (List Project)
projectsDecoder =
    D.list projectDecoder


projectDecoder : Decoder Project
projectDecoder =
    D.map6 Project
        (D.field "id" D.int)
        (D.at [ "namespace", "full_path" ] D.string)
        (D.field "name" D.string)
        (D.field "description" blankable)
        (D.field "web_url" D.string)
        (D.field "last_activity_at" <| Iso8601.decoder)


blankable : Decoder (Maybe String)
blankable =
    D.string
        |> D.nullable
        |> D.map
            (\str ->
                case str of
                    Just "" ->
                        Nothing

                    dunno ->
                        dunno
            )


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
