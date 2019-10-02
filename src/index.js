import {Elm} from "./elm/Main.elm";

function log(msg) {
    if (process.env.NODE_ENV !== 'production') {
        console.log(msg);
    }
}

const app = Elm.Main.init({
    flags: {
        gitlabHost: process.env.GITLAB_HOST,
        gitlabAppId: process.env.GITLAB_APP_ID
    }
});

app.ports.log.subscribe(log);