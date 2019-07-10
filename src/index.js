import {Elm} from "./elm/Main.elm";


Elm.Main.init({
    flags: {
        gitlabUrl: process.env.GITLAB_URL,
        gitlabProject: parseInt(process.env.GITLAB_PROJECT)
    }
});