import { Elm } from './Main.elm';
import Peer from 'peerjs';

// var seedArray = new Uint32Array(1);
// self.crypto.getRandomValues(seedArray);

app = Elm.Main.init({ node: document.getElementById('root') });
app.peer = null;

app.ports.initConnection.subscribe((id) => {
  app.peer = new Peer(id);
  console.log('startHosting, peer id: ' + app.peer.id);
  app.ports.peerId.send(peer.id);
})

app.ports.startHosting.subscribe((args) => {
  app.peer.on('open', (id) => {
    console.log('connection id: ' + id);
    app.ports.connectionDescription.send(id);
  });

  app.peer.on("error", (err) => console.log(err));
});
