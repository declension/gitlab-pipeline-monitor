module Model exposing (Flags, GitRef, GitlabData, Host, Model, Msg(..), Pipeline, PipelineStore, Project, ProjectId, Status(..), Token)

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


type alias GitRef =
    String


type alias Pipeline =
    { ref : String
    , id : Int
    , status : Status
    , url : String
    }


type alias Project =
    { id : Int
    , namespace : String
    , name : String
    , description : Maybe String
    , url : String
    , lastActivity : Posix
    }


type alias PipelineStore =
    Dict ProjectId (Dict GitRef (List Pipeline))


type alias GitlabData =
    { projects : List Project
    , pipelines : Dict ProjectId (List Pipeline)
    }


type alias Host =
    String


type alias Flags =
    { gitlabHost : Host
    , gitlabAppId : String
    }


type Status
    = Running
    | Success
    | Failed
    | Pending
    | Cancelled


type Msg
    = Tick Posix
    | LinkClicked Browser.UrlRequest
    | GotPipelinesFor ProjectId (Result Http.Error (List Pipeline))
    | GotProjects (Result Http.Error (List Project))
    | UrlChanged Url.Url
