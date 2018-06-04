#/usr/bin/sh
elm-make src/Blargl.elm --output=blargl.js
minify blargl.js > blargl.min.js
