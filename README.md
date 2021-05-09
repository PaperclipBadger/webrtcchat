### webrtcchat

Compile and run:
    
    elm make src/Main.elm --output=main.js
    python -m http.server 5000

You also need to be running a peerjs server on localhost:9000

    npm install peer@0.6.1 -g
    peerjs --port 9000 --path /
