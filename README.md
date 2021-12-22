# ebms-admin-elm

## Install nvm
``` bash
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
nvm install --lts
nvm install-latest-npm
```
## Configure npm
``` bash
mkdir ~/.npm-global
printf "export NPM_CONFIG_PREFIX=~/.npm-global" >> ~/.profile
printf "export PATH=~/.npm-global/bin:$PATH" >> ~/.profile
. ~/.profile
```
## Install elm
``` bash
npm install -g elm elm-test elm-format
```
## Start elm server (only for development)
``` bash
elm reactor
```
## Install elm-live
``` bash
npm install -g elm-live
```
## Watch files
``` bash
elm-live src/Main.elm src/Home.elm src/About.elm -- --output=elm.js
```
## Install http-server
``` bash
npm install -g http-server-spa
```
## Run http-server
``` bash
http-server-spa .
```
