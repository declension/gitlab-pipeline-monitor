module Model exposing (Flags, Model, Msg(..), Pipeline, Status(..), Token)

import Browser
import Browser.Navigation as Nav
import Http
import Url


type alias Token =
    String


type alias Model =
    { config : Flags
    , key : Nav.Key
    , token : Maybe Token
    , pipelines : List Pipeline
    , url : Url.Url
    }


type alias Flags =
    { gitlabHost : String
    , gitlabProject : Int
    , gitlabAppId : String
    }


type Status
    = Success
    | Failed
    | Pending
    | Cancelled
    | Unknown


type alias Pipeline =
    { ref : String
    , id : Int
    , status : Status
    , url : String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | GotProjects (Result Http.Error (List Pipeline))
    | UrlChanged Url.Url
