// This file starts livereload service (secure websockets server). Browser
// clients connect to the server and if a file changes, a page reload request
// is pushed to the browser so it refreshes automatically.

try {
    require('livereload');
}
catch (e) {
    console.error(
        'Dependency livereload not found. Try doing `npm link livereload`.');
    process.exit(1);
}
const livereload = require('livereload');
const fs = require('fs');
const path = require('path');

// Check if all args are provided
if (process.argv[2] === undefined) {
    usage()
}
if (process.argv[3] === undefined) {
    usage()
}
if (process.argv[4] === undefined) {
    usage()
}

var watchedDir = process.argv[2];
var mode = process.argv[3];
var serverCert = process.argv[4];

// Use provider certs as certs for livereload websocket server.
const options = {
    key: fs.readFileSync(serverCert),
    cert: fs.readFileSync(serverCert)
};

// Check if resolved path is a directory
watchedDir = path.resolve(watchedDir);
var isDir = false;
try {
    var stats = fs.lstatSync(watchedDir);
    if (stats.isDirectory()) {
        isDir = true;
    }
}
catch (e) {
}

// Check mode
var usePolling = false;
if (mode === 'poll') {
    usePolling = true;
}

// Start livereload if given dir is ok
if (isDir) {
    server = livereload.createServer({https: options, usePolling: usePolling});
    server.watch(watchedDir);
    console.log("Successfully started livereload in directory " + watchedDir);
} else {
    console.error('Failed to start livereload. Directory `' +
        watchedDir + '` does not exist.');
    process.exit(1);
}

function usage() {
    console.error(
        'Usage: node gui_livereload.js <directory> <mode> <server_cert>\n' +
        '   directory - dir to be watched for changed files\n' +
        '   mode - watch or poll\n' +
        '       watch - use FS watcher, does not work on network filesystems\n' +
        '       poll - poll for changes, slower but works everywhere\n' +
        '   server_cert - certificate used for https livereload server');
    process.exit(1);
}