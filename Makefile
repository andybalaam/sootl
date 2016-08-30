
all: compile

compile:
	elm-make --output=sootl.js Main.elm

run:
	elm-reactor

setup:
	sudo npm install -g elm@0.17.1
	elm-package install
