import {Elm} from "./elm/Main.elm";


Elm.Main.init({
    flags: {
        gitlabHost: process.env.GITLAB_HOST,
        gitlabAppId: process.env.GITLAB_APP_ID,
        startupTime: new Date().getTime()
    }
});