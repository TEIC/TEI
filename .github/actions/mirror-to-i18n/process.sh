#!/bin/bash

git config --global user.email "$ACTION_USER_EMAIL"
git config --global user.name "$ACTION_USER_NAME"

mkdir -p /root/.ssh
echo "$SSH_PRIVATE_KEY" > /root/.ssh/id_rsa
chmod 600 /root/.ssh/id_rsa

SOURCE=`pwd`

git clone git@github.com:TEIC/I18n-TEI.git /opt/I18n-TEI
cd /opt/I18n-TEI
git rebase $SOURCE dev
git push origin main
