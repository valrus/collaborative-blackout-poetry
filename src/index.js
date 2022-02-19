import { Elm } from './Main.elm';
import Peer from 'peerjs';

function generateGameId(length) {
  var seedArray = new Uint32Array(20);
  crypto.getRandomValues(seedArray);
  return String.fromCharCode(...seedArray.map(n => 97 + (n % 26)));
}
var gameId = generateGameId(20);

app = Elm.Main.init({ node: document.getElementById('root'), flags: gameId });
app.peer = new Peer(gameId);

app.ports.connectToHost.subscribe((hostId) => {
  var connection = peer.connect(hostId);
  // on open will be called when you successfully connect to PeerServer
  connection.on('open', function(){
    app.ports.connectedAsGuest.send(connection.id);
    // connection.send('hi!');
  });
})

// app.ports.startHosting.subscribe((args) => {
//   app.peer.on('open', (gameId) => {
//     console.log('connection gameId: ' + gameId);
//     app.ports.connectionDescription.send(gameId);
//   });

//   app.peer.on("error", (err) => console.log(err));
// });
