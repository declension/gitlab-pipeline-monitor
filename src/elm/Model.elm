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

type alias Host = String

type alias Flags =
    { gitlabHost : Host
    , gitlabProject : Int
    , gitlabAppId : String
    }


type Status
    = Running
    | Success
    | Failed
    | Pending
    | Cancelled


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
    =
    Tick Posix
    | LinkClicked Browser.UrlRequest
    | GotPipelinesFor ProjectId (Result Http.Error (List Pipeline))
    | GotProjects (Result Http.Error (List Project))
    | UrlChanged Url.Url
