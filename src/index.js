import {Elm} from "./elm/Main.elm";


Elm.Main.init({
    flags: {
        gitlabHost: process.env.GITLAB_HOST,
        gitlabProject: parseInt(process.env.GITLAB_PROJECT),
        gitlabAppId: process.env.GITLAB_APP_ID
    }
});