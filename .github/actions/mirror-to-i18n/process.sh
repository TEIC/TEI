#!/bin/bash

git config --global user.email "$ACTION_USER_EMAIL"
git config --global user.name "$ACTION_USER_NAME"

mkdir -p /root/.ssh
echo "$SSH_PRIVATE_KEY" > /root/.ssh/id_rsa
chmod 600 /root/.ssh/id_rsa

SOURCE=`pwd`

mkdir /opt/I18n-TEI
git clone https://github.com/TEIC/I18n-TEI.git /opt/I18n-TEI
cd /opt/I18n-TEI
git remote add upstream https://github.com/TEIC/TEI.git
git pull --rebase upstream dev
git push origin main
