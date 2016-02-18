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

// Use provider certs as certs for livereload websocket server.
const options = {
    key: fs.readFileSync('/root/bin/node/etc/certs/onedataServerWeb.pem'),
    cert: fs.readFileSync('/root/bin/node/etc/certs/onedataServerWeb.pem')
};

// Resolve directory to watch based on first argument (defaults to cwd)
var watchedDir = process.argv[2];
if (watchedDir === undefined) {
    watchedDir = __dirname
}
watchedDir = path.resolve(watchedDir);
// Check if resolved path is a directory
var isDir = false;
try {
    var stats = fs.lstatSync(watchedDir);
    if (stats.isDirectory()) {
        isDir = true;
    }
}
catch (e) {
}

// Start livereload if given dir is ok
if (isDir) {
    server = livereload.createServer({https: options});
    server.watch(watchedDir);
    console.log("Successfully started livereload in directory " + watchedDir);
} else {
    console.error('Failed to start livereload. Directory `' +
        watchedDir + '` does not exist.');
    process.exit(1);
}
