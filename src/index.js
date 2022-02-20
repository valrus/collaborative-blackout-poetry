import { Elm } from './Main.elm';
import Peer from 'peerjs';

function generateGameId(length) {
    var seedArray = new Uint32Array(length);
    crypto.getRandomValues(seedArray);
    return String.fromCharCode(...seedArray.map(n => 97 + (n % 26)));
}
var gameId = generateGameId(20);

var app = Elm.Main.init({ node: document.getElementById('root'), flags: gameId });
app.peer = new Peer(gameId);
// when connecting as a guest
app.hostConnection = null;
// when hosting
app.guestConnections = {};

function setupMessageReceipt(connection) {
    connection.on('data', (data) => {
        console.log('received data: ' + data);
        app.ports.receivedMessage.send({
            fromConnection: connection.peer,
            data: data
        });
    });
}

// connecting as a guest
app.ports.connectToHost.subscribe((hostId) => {
    app.guestConnections = {};
    app.hostConnection = app.peer.connect(hostId);

    // on open will be called when you successfully connect to PeerServer
    app.hostConnection.on('open', function() {
        setupMessageReceipt(app.hostConnection);
        app.ports.sendAsGuest.subscribe((data) => {
            console.log(data);
            app.hostConnection.send(data);
        });

        app.ports.connectedAsGuest.send(app.hostConnection.id);
    });
})

// connecting as the host
// TODO: what's args?
app.ports.startHosting.subscribe((_args) => {
    // reset connections if we were previously connected to another game
    app.hostConnection = null;
    app.guestConnections = {};

    app.peer.on('connection', (connection) => {
        setupMessageReceipt(connection);
        app.guestConnections[connection.peer] = connection;

        app.ports.guestConnected.send(connection.label);
    });

    app.ports.sendAsHost.subscribe((data) => {
        Object.entries(app.guestConnections).forEach(([_peerId, connection]) => {
            connection.send(data);
        });
    })

    app.peer.on("error", (err) => console.log(err));
});
