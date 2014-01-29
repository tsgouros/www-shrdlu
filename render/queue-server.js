var sys = require("sys");
var http = require('http');
var url = require('url');
var path = require('path');
var fs = require('fs');
var spawn = require('child_process').spawn;

if (process.argv.length < 5) {
    console.log("usage: node queue-server.js host port shrdluScript [debug]");
    return;
}

var host = process.argv[2];
var port = process.argv[3];
var shrdluScript = process.argv[4];

var debug;
if (process.argv.length > 5) {
    debug = true;
} else {
    debug = false;
}

// This will be restarted as soon as someone asks for the
// icon-shrdlu.png button image, so it doesn't have to run detached
// yet.  Need a command-line arg to turn it off or on.
var shrdluProcess = spawn(shrdluScript, [host, port]);

if (debug) {
    shrdluProcess.stdout.on('data', function (data) {
	console.log('stdout: ' + data);
    });

    shrdluProcess.stderr.on('data', function (data) {
	console.log('stderr: ' + data);
    });

    shrdluProcess.on('close', function (code) {
	console.log('child process exited with code ' + code);
    });
}

var moveQueue = Array();
var cmdQueue = Array();
var resQueue = Array();

var counter = 0;
var outstring = '-empty-';

http.createServer(function (request, response) {
    var pathname = url.parse(request.url).pathname;
    var queryData  = url.parse(request.url, true).query;

    counter++;

    if (pathname == "/") {
	pathname = "/index.html";
    }

    // console.log('processing request (' + counter.toString() + '): ' + pathname);
    // console.log('act-->' + queryData.act);
    // console.log('actget-->' + queryData.actget);
    // console.log('cmd-->' + queryData.cmd);
    // console.log('cmdget-->' + queryData.cmdget);
    // console.log('res-->' + queryData.res);
    // console.log('resget-->' + queryData.resget);

    var out;

    if (queryData.act) {

	moveQueue.push(queryData.act);

	out = queryData.act;

    } else if (queryData.actget) {

	if (moveQueue.length > 0) {

	    out = moveQueue[0];
	    moveQueue.splice(0, 1);

	} else {

	    out = outstring;

	}
    } else if (queryData.cmd) {

	cmdQueue.push(queryData.cmd);

	out = queryData.cmd;

    } else if (queryData.cmdget) {

	if (cmdQueue.length >0) {

	    out = cmdQueue[0];
	    cmdQueue.splice(0, 1);

	} else {

	    out = outstring;

	}

    } else if (queryData.res) {

	resQueue.push(queryData.res);

	out = queryData.res;

    } else if (queryData.resget) {

	if (resQueue.length >0) {

	    out = resQueue[0];
	    resQueue.splice(0, 1);

	} else {

	    out = outstring;

	}

    } else {

	// No query string.  Must just want a file.
	var filename = path.join(process.cwd(), pathname);

	// If we want the shrdlu icon, restart the shrdlu process
	// because we're restarting the display window.
	if (/icon-shrdlu.png/g.test(filename)) {

	    shrdluProcess.kill();

	    shrdluProcess = spawn(shrdluScript, [host, port], 
				  {detached: true, stdio: ['ignore']} );

	    if (debug) {
		shrdluProcess.stdout.on('data', function (data) {
		    console.log('stdout: ' + data);
		});

		shrdluProcess.stderr.on('data', function (data) {
		    console.log('stderr: ' + data);
		});

		shrdluProcess.on('close', function (code) {
		    console.log('child process exited with code ' + code);
		});
	    }


	    shrdluProcess.on('error', function (err) { 
		console.log(">>>" + err); } );

	    shrdluProcess.unref();

	}

	fs.exists(filename, function(exists) {
    	    if (!exists) {
    		response.writeHead(404, {"Content-Type": "text/plain"});
    		response.end("404 Not Found\n");
    	    } else {
    		fs.readFile(filename, "binary", function(err, file) {
    		    if(err) {
    			response.writeHead(500, {"Content-Type": "text/plain"});
    			response.end(err + "\n");
    		    } else {

			if (filename.match(/\.js/g)) {
			    response.writeHead(200, {"Content-Type": "application/javascript"});
			    response.write(file, "utf8");

			} else if (filename.match(/\.css/g)) {
			
			    response.writeHead(200, {"Content-Type": "text/css"});
			    response.write(file, "utf8");

			} else if (filename.match(/\.png/g)) {
			
			    response.writeHead(200, {"Content-Type": "image/png"});
			    response.write(file, "binary");

			} else {

			    response.write(file, "utf8");
			}
    			response.end();
		    }
		});
	    }
	});

	out = '';
    }

    // console.log("moveQueue");
    // console.log(moveQueue);
    // console.log("cmdQueue");
    // console.log(cmdQueue)
    // console.log("resQueue");
    // console.log(resQueue)

    if (out) {
	response.writeHead(200, {'Content-Type': 'text/plain'});
	response.end(out + '\n');
    }

}).listen(port, host);

console.log('Server running at http://' + host + ":" + port);