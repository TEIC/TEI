#!/bin/bash

git config --global user.email "$ACTION_USER_EMAIL"
git config --global user.name "$ACTION_USER_NAME"

mkdir /opt/I18n-TEI
git clone "https://tei-bot:$GITHUB_ACCESS_TOKEN@github.com/TEIC/I18n-TEI.git" /opt/I18n-TEI
cd /opt/I18n-TEI
git remote add upstream https://github.com/TEIC/TEI.git
git pull --rebase upstream dev
git push origin main
