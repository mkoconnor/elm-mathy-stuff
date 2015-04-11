#!/bin/sh

elm-make Doodle.elm --output doodle.js
scp Doodle.elm doodle.js doodle.html birdglue:public_html/doodle
