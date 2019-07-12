Gitlab CI Monitor
=================

What?
-----
A modern, big-screen-friendly UI for monitoring Gitlab Pipelines (CI).

Using the magic of [Elm](https://elm-lang.org/) and the Gitlab v4 API.


Why?
----
### Aren't there already plenty of these?
Yes, but, err _this one's different_, etc etc.

### How so?
Shush. Erm, WebGL?


How?
----

### Build
```bash
yarn
```

### Run
1. Register a new app in your Gitlab UI e.g. https://gitlab.example.com/profile/applications
1. Edit the [`.env`](./.env) file in the root, and fill in the endpoint for your Gitlab hostname (assumes port 443 for now), the project ID to watch and the newly created Application ID.
1. Run the dev server (with HMR):
```
yarn serve
```

### Wow, that was fast
Yes! Elm 0.19 + Yarn + [ParcelJS](https://parceljs.org/) == :rocket: 


When?
-----

Err, it'll be a while yet before anything good is finished. It's like `v0.0.2`.
