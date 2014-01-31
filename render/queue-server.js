var sys = require("sys");
var http = require('http');
var url = require('url');
var path = require('path');
var fs = require('fs');
var spawn = require('child_process').spawn;
var sessions = require('./session.js');


if (process.argv.length < 5) {
    console.log("usage: node queue-server.js host port shrdluScript [debug]");
    return;
}

var host = process.argv[2];
var port = process.argv[3];
var shrdluScript = process.argv[4];

var session = sessions.create({ "domain": host, "port": port });

var debug;
if (process.argv.length > 5) {
    debug = true;
} else {
    debug = false;
}


    // check out shrdluScript
fs.stat(shrdluScript, function (err, stats) { 
    try {
	if (!(stats.isFile() && 
	      (1 & parseInt((stats.mode & parseInt("777", 8))
			    .toString(8)[0])))) {
	    console.log(shrdluScript + " not executable.");
	    process.exit(1);
	}
    } catch(err) {
	console.log("problem with file " + shrdluScript + ".  Probably not found ");
	process.exit(1);
    }
});
// This will be restarted as soon as someone asks for the index.html
// file, so it doesn't have to run detached yet.  This process will
// never be used, but it's less trouble to start it here and kill it
// later.
var shrdluProcess = spawn(shrdluScript, [host, port]);
// tbd: Need a command-line arg to turn it off or on.

// if (debug) {
//     shrdluProcess.stdout.on('data', function (data) {
// 	console.log('stdout1: ' + data);
//     });

//     shrdluProcess.stderr.on('data', function (data) {
// 	console.log('stderr1: ' + data);
//     });

//     shrdluProcess.on('close', function (code) {
// 	console.log('child process exited with code ' + code);
//     });
// }


var moveQueue = Array();
var cmdQueue = Array();
var resQueue = Array();

var counter = 0;
var outstring = '-empty-';

var prevConsoleString = "";
var prevConsoleCount = 0;
function printConsole(str) {
    if (str.localeCompare(prevConsoleString) == 0) {
	prevConsoleCount++;
	process.stdout.write(str + " (" + prevConsoleCount.toString() + ")\r");
    } else {
	prevConsoleCount = 0;
	process.stdout.write("\n" + str + "\r");
    }

    prevConsoleString = str;
}

http.createServer(function (request, response) {
    var pathname = url.parse(request.url).pathname;
    var queryData  = url.parse(request.url, true).query;

    // console.log(request.headers);
    // console.log("****" + url.parse(request.url).pathname);
    // console.log("^^^^" + request.url + "<<<");

    // for (var entry in queryData) {
    // 	console.log("+++" + entry + "->(" + queryData[entry] + ")"); }

    // console.log(Object.keys(queryData).length);

    var cookies = {};
    request.headers.cookie && request.headers.cookie.split(';').forEach(function(cookie) {
	var parts = cookie.split('=');
	cookies[ parts[0].trim() ] = ( parts[1] || '').trim();

	if (debug) 
	    console.log("cookie found: " + parts[0].trim() + " = " + (parts[1] || '').trim());
    });

    counter++;

    if ((pathname == "/") && (Object.keys(queryData).length == 0)) {
	pathname = "/index.html";
    }

    if (debug) {

	if (moveQueue.length > 0) 
	    printConsole("moveQueue:[" + moveQueue.toString() + "]");
	if (cmdQueue.length > 0)
	    printConsole("cmdQueue:[" + cmdQueue.toString() + "]");
	if (resQueue.length > 0)
	    printConsole("resQueue:[" + resQueue.toString() + "]");

	if ( !(queryData.act === undefined))
	    printConsole('act-->' + queryData.act);
	if ( !(queryData.actget === undefined))
	    printConsole('actget-->' + queryData.actget);
	if ( !(queryData.cmd === undefined))
	    printConsole('cmd-->' + queryData.cmd);
	if ( !(queryData.cmdget === undefined))
	    printConsole('cmdget-->' + queryData.cmdget);
	if ( !(queryData.res === undefined))
	    printConsole('res-->' + queryData.res);
	if ( !(queryData.resget === undefined))
	    printConsole('resget-->' + queryData.resget);
    }

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

	// If we want the index.html, restart the shrdlu process
	// because we're restarting the display window.
	if (/index.html/g.test(filename)) {

	    shrdluProcess.kill();

	    shrdluProcess = spawn(shrdluScript, [host, port], 
				  {detached: true, stdio: ['ignore']} );

	    if (debug) {
		shrdluProcess.stdout.on('data', function (data) {
		    printConsole('stdout: ' + data);
		});

		shrdluProcess.stderr.on('data', function (data) {
		    printConsole('stderr: ' + data);
		});

		shrdluProcess.on('close', function (code) {
		    printConsole('child process exited with code ' + code);
		});
	    }

	    shrdluProcess.on('error', function (err) { 
		printConsole(">>>" + err); } );

	    shrdluProcess.unref();

	    // Renew the cookies
	    session = sessions.lookupOrCreate(request, {
		lifetime: 86400,
		domain: host,
		port: port
	    });

	    if (debug) {
		printConsole("cookie: " + session.getSetCookieSessionValue());
		printConsole("cookie: " + session.getSetCookiePortValue());
	    }

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

			var fileType, contentType;

			if (filename.match(/\.js/g)) {
			    contentType = "application/javascript";
			    fileType = "utf8";

			} else if (filename.match(/\.css/g)) {
			    contentType = "text/css";
			    fileType = "utf8";

			} else if (filename.match(/\.png/g)) {
			    contentType = "image/png";
			    fileType = "binary";

			} else {
			    contentType = "text/html";
			    fileType = "utf8";
			}

			if (debug) {
			    printConsole("sending: " + filename + " (" + fileType + ", " + contentType + ")");
			    printConsole("cookie:" + session.getSetCookieSessionValue());
			    printConsole("cookie:" + session.getSetCookiePortValue());
			}

			response.writeHead(200, [
			    ["Content-Type", contentType ],
			    ["Set-Cookie", session.getSetCookieSessionValue()],
			    ["Set-Cookie", session.getSetCookiePortValue() ]
			]);

			response.write(file, fileType);
    			response.end();
		    }
		});
	    }
	});

	out = '';
    }

    if (out) {
	response.writeHead(200, [
	    ["Content-Type", "text/plain" ],
	    ["Set-Cookie", session.getSetCookieSessionValue()],
	    ["Set-Cookie", session.getSetCookiePortValue() ]
	]);

	response.end(out + '\n');
    }

}).listen(port, host);

console.log('Server running at http://' + host + ":" + port);