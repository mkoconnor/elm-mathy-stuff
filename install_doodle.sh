#!/bin/sh

elm-make Doodle.elm --output doodle.js
scp Doodle.elm doodle.js birdglue:public_html/doodle
scp doodle.html birdglue:public_html/doodle/index.html
