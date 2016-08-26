
all: compile

compile:
	elm-make --output=sootl.js Main.elm

run:
	elm-reactor

setup:
	elm-package install
