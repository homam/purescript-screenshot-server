# Installation

Configure ecosystem.config.js 

Env variables:

```
osui_aws_access_key_id: ""
osui_secret_access_key: ""
```

Build:

```
yarn
yarn build-ts
spago build

pm2 ecosystem.config.js
```


# Usage

```
http://localhost:8080/?url=https://nytimes.com
```

Optional parameters:

* `ft`: number, fresher than this many seconds before now
* `redir`: `true`, redirect to the screenshot