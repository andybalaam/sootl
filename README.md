# Stay out of the light!

A fun way for me to learn Elm.

One day it might be a game where you have to avoid the lights.

For now it might be helpful as an example of how to build an SVG that resizes
when the screen changes, in Elm.

Play: https://andybalaam.github.io/sootl/

## Build

    sudo apt-get install nodejs-legacy npm
    sudo npm install -g elm
    sudo npm install -g elm-live

    git clone https://github.com/andybalaam/sootl.git
    cd sootl
    elm-package install

    elm-live Main.elm --ouput=sootl.js --open

## License

Sootl is Copyright (C) 2016 Andy Balaam.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

See [LICENSE.md](LICENSE.md) for details.

