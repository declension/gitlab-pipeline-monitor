module Wire exposing (emptyHttps, extractToken, getUrl, pipelineDecoder, pipelinesDecoder, pipelinesUrl, projectsDecoder, statusDecoder, toToken)

import Http exposing (emptyBody, expectJson, header)
import Json.Decode as D exposing (Decoder)
import Model exposing (Flags, Msg(..), Pipeline, Status(..), Token)
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


pipelinesUrl : Flags -> Url
pipelinesUrl flags =
    { emptyHttps
        | host = flags.gitlabHost
        , path = "/api/v4/projects/" ++ String.fromInt flags.gitlabProject ++ "/pipelines"
        , query = Just <| stripQuestion <| toQuery [ Builder.int "per_page" 30 ]
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


projectsDecoder : Decoder (List String)
projectsDecoder =
    D.list (D.field "name" D.string)


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
