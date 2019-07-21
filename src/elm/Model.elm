module Model exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Http
import Time exposing (Posix)
import Url


type alias Token =
    String


type alias Model =
    { config : Flags
    , key : Nav.Key
    , token : Maybe Token
    , data : GitlabData
    , url : Url.Url
    }


type alias ProjectId =
    Int


type alias GitlabData =
    { projects : List Project
    , pipelines : Dict ProjectId (List Pipeline)
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


type alias Project =
    { id : Int
    , namespace: String
    , name : String
    , description : Maybe String
    , url : String
    , lastActivity : Posix
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | GotPipelinesFor ProjectId (Result Http.Error (List Pipeline))
    | GotProjects (Result Http.Error (List Project))
    | UrlChanged Url.Url
