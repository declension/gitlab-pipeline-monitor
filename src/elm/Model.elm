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
    , now : Posix
    }


type alias ProjectId =
    Int


type alias PipelineId =
    Int


type alias GitRef =
    String


type alias GitSha =
    String


type alias PipelineStore =
    Dict ProjectId (Dict GitRef (Dict PipelineId Pipeline))


type alias GitlabData =
    { projects : List Project
    , pipelines : PipelineStore
    }


type alias Host =
    String


type alias Flags =
    { gitlabHost : Host
    , gitlabAppId : String
    , startupTime: Int
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
    , detail : Maybe PipelineDetail
    }


type alias User =
    { name : String
    , username : String
    , avatarUrl : Maybe String
    }


type alias PipelineDetail =
    { id: PipelineId
    , sha : GitSha
    , user : User
    , createdAt : Posix
    , startedAt : Maybe Posix
    , finishedAt : Maybe Posix
    }


type alias Project =
    { id : ProjectId
    , namespace : String
    , name : String
    , description : Maybe String
    , url : String
    , lastActivity : Posix
    }


type Msg
    = FetchProjects Posix
    | LinkClicked Browser.UrlRequest
    | GotPipelinesFor ProjectId (Result Http.Error (List Pipeline))
    | GotPipelineDetailFor ProjectId GitRef (Result Http.Error (PipelineDetail))
    | GotProjects (Result Http.Error (List Project))
    | UrlChanged Url.Url
