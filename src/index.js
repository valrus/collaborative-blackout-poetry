import { Elm } from './Main.elm';
import Peer from 'peerjs';

function generateGameId(length) {
    var seedArray = new Uint32Array(length);
    crypto.getRandomValues(seedArray);
    return String.fromCharCode(...seedArray.map(n => 97 + (n % 26)));
}

const startingGameId = generateGameId(20);

var app = Elm.Main.init({ node: document.getElementById('root'), flags: startingGameId });

function initPeer(gameId) {
    if (app.peer && !app.peer.destroyed) {
        console.log('destroying peer')
        app.peer.destroy();
    }

    var newGameId = gameId || generateGameId(20);
    console.log('new peer with gameId: ' + newGameId);
    app.peer = new Peer(newGameId);

    // when connecting as a guest
    app.hostConnection = null;
    // when hosting
    app.guestConnections = {};
}

app.ports.init.subscribe((gameId) => {
    initPeer(gameId);
});

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
    console.log(app.peer);
    app.hostConnection = app.peer.connect(hostId);
    console.log(app.hostConnection);

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

    app.ports.reset.subscribe((playerName) => {
        console.log('heard reset for ' + playerName);
        if (app.hostConnection.open && playerName) {
            console.log('sending disconnect to ' + app.hostConnection.peer);
            // guest hit the back button. tell the host, then reset
            app.hostConnection.send('test');
            app.hostConnection.send({disconnection: playerName});
            // this sucks, but
            // https://github.com/peers/peerjs/issues/634
            setTimeout(() => {
                app.hostConnection.close();
                initPeer(null);
            }, 500);
            window.location.reload();
        } else {
            // if playerName is blank, the host disconnected. no need to tell anyone, just reset
            initPeer(null);
            window.location.reload();
        }
    })
})

// connecting as the host
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

    app.ports.reset.subscribe((_playerName) => {
        // host hit the back button. tell all players, then reset
        Object.entries(app.guestConnections).forEach(([_peerId, connection]) => {
            connection.send({"disconnection": null});
        });
        initPeer(null);
        window.location.reload();
    })

    // TODO: send error back to elm
    app.peer.on("error", (err) => {console.log(err)});

    // TODO: send successful host setup back to elm
});
