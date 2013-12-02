var http = require('http');
var url = require('url');
var moveQueue = Array();
var cmdQueue = Array();

http.createServer(function (request, response) {
    var pathname = url.parse(request.url).pathname;
    var queryData  = url.parse(request.url, true).query;

    console.log('processing request' + pathname + queryData);
    console.log('act-->' + queryData.act);
    console.log('actget-->' + queryData.actget);
    console.log('cmd-->' + queryData.cmd);
    console.log('cmdget-->' + queryData.cmdget);

    var out;

    if (queryData.act) {

	moveQueue.push(queryData.act);

	out = queryData.act;

    } else if (queryData.actget) {

	if (moveQueue.length > 0) {

	    out = moveQueue[0];
	    moveQueue.splice(0, 1);

	} else {

	    out = 'empty';

	}
    } else if (queryData.cmd) {

	cmdQueue.push(queryData.cmd);

	out = queryData.cmd;

    } else if (queryData.cmdget) {

	if (cmdQueue.length >0) {

	    out = cmdQueue[0];
	    cmdQueue.splice(0, 1);

	} else {

	    out = 'empty';

	}
    } else {

	out = 'error';

    }

    console.log("moveQueue");
    console.log(moveQueue);
    console.log("cmdQueue");
    console.log(cmdQueue)

    response.writeHead(200, {'Content-Type': 'text/plain'});
    response.end(out + '\n');

}).listen(1337, '127.0.0.1');

console.log('Server running at http://127.0.0.1:1337/');