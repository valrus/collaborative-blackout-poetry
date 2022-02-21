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
    console.log('setting up msg receipt for ' + connection.peer);
    connection.on('data', (data) => {
        console.log('received data: ' + data);
        app.ports.receivedMessage.send(data);
    });
}

// connecting as a guest
app.ports.connectToHost.subscribe((hostId) => {
    app.guestConnections = {};
    app.hostConnection = app.peer.connect(hostId);

    // on open will be called when you successfully connect to PeerServer
    app.hostConnection.on('open', function() {
        console.log("connected to: " + app.hostConnection.peer);
        setupMessageReceipt(app.hostConnection);
        app.ports.sendAsGuest.subscribe((data) => {
            console.log("sendAsGuest, guestName: " + data.guestName);
            app.hostConnection.send(data);
        });

        app.ports.connectedAsGuest.send(app.hostConnection.peer);
    });
})

// connecting as the host
// TODO: what's args?
app.ports.startHosting.subscribe(() => {
    console.log('init hosting');
    // reset connections if we were previously connected to another game
    app.hostConnection = null;
    app.guestConnections = {};

    console.log('starting hosting');
    app.peer.on('connection', (connection) => {
        console.log('new peer: ' + connection.label);
        setupMessageReceipt(connection);
        app.guestConnections[connection.peer] = connection;
    });

    app.ports.sendAsHost.subscribe((data) => {
        Object.entries(app.guestConnections).forEach(([_peerId, connection]) => {
            connection.send(data);
        });
    })

    app.peer.on("error", (err) => console.log(err));
});
