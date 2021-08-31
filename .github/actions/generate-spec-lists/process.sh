#!/bin/bash

git config --global user.email "$ACTION_USER_EMAIL"
git config --global user.name "$ACTION_USER_NAME"

mkdir /opt/spec_translator
git clone "https://tei-bot:$GITHUB_ACCESS_TOKEN@github.com/TEIC/spec_translator.git" /opt/spec_translator

cd P5
make p5.xml
saxon -s:p5subset.xml -xsl:Utilities/generate_i18n_spec_lists.xsl -o:/opt/spec_translator/docs/js/spec_lists.json

cd /opt/spec_translator
git add docs/js/spec_lists.json
git commit -m "Updated spec lists."
git push origin main