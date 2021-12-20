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
## Start elm server
``` bash
elm reactor
```