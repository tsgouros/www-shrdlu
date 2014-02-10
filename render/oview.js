// To do list:

// Change so that coordinates refer to a corner, as in shrdlu
// Implement create object command,
// Have shrdlu populate the screen
// connect mover.lisp

var stdColors = {"aqua":"#00ffff","black":"#000000","blue":"#0000ff",
		 "brown":"#a52a2a", "chartreuse":"#7fff00",
		 "cyan":"#00ffff", "darkblue":"#00008b",
		 "darkcyan":"#008b8b", "darkgray":"#a9a9a9",
		 "darkgreen":"#006400", "darkmagenta":"#8b008b",
		 "darkorange":"#ff8c00", "darkred":"#8b0000",
		 "darkviolet":"#9400d3", "dimgray":"#696969",
		 "fuchsia":"#ff00ff", "gold":"#ffd700",
		 "gray":"#808080", "green":"#00ff00", "indigo ":"#4b0082",
		 "ivory":"#fffff0", "lavender":"#e6e6fa",
		 "lightcyan":"#e0ffff", "lightgrey":"#d3d3d3",
		 "lightgreen":"#90ee90", "lightyellow":"#ffffe0",
		 "magenta":"#ff00ff", "maroon":"#800000",
		 "mediumblue":"#0000cd", "mediumpurple":"#9370d8",
		 "navy":"#000080", "olive":"#808000", "orange":"#ffa500",
		 "pink":"#ffc0cb", "purple":"#800080", "red":"#ff0000",
		 "royalblue":"#4169e1", "silver":"#c0c0c0",
		 "skyblue":"#87ceeb", "tan":"#d2b48c", "violet":"#ee82ee",
		 "white":"#ffffff", "yellow":"#ffff00"};

var container, stats;
var camera, scene, renderer, projector;
var objs = new blockCollection();
var cmds = new commands();

var targetRotation = 2.3;
var targetRotationOnMouseDown = 2.3;

var pickupHeight = 125;

init();
animate();

// This object interprets text commands into object motion commands.
// The text commands are to be issued by a SHRDLU port and are in the
// SHRDLU coordinates.
//
// SHRDLU coordinates: x runs along the front of the screen, y is
// depth, and z is height.  This means that to convert from SHRDLU to
// three.js, {x: shrdlu.y, y: shrdlu.z, z: shrdlu.x}.  Also, the
// control point of the shapes in SHRDLU is the lower left corner (and
// nothing is ever rotated).
function commands() {
    // commands are these:
    //  CREATE/NAME/TYPE/COLOR/DIMX/DIMY/DIMZ/POSX/POSY/POSZ
    //  REPLACE/NAME/POSX/POSY/POSZ
    //  QUERY/NAME
    //  MOVE/POSX/POSY/POSZ
    //  GRASP
    //  RELEASE
    //
    // The MOVE, GRASP, and RELEASE commands are pretty much straight
    // out of SHRDLU.  The CREATE, QUERY, and REPLACE commands exist
    // to set the scene in the first place and to recover from errors
    // (i.e. mismatches between the display and the memory).

    // Tracks the location of SHRDLU's invisible hand.  The GRASP
    // command only picks up stuff that's immediately beneath the
    // hand.
    this.hand = {x: 0, y: 200, z: 0};

    // Points to the block object in SHRDLU's hand.
    this.objInHand = null;

    // Parses a command string into the methods and arguments.
    this.parse = parse;

    // The methods corresponding to each command.
    this.create = create;
    this.move = move;
    this.grasp = grasp;
    this.release = release;
    this.blink = blink;

    // Parses a command string into a method and its arguments, and
    // invokes the methods on the arguments.
    function parse(string)
    {
	cmdArray = string.split("/");

	switch ( cmdArray[0] )
	{
	case "CREATE":
	    this.create(cmdArray[1], cmdArray[2], cmdArray[3],
			{x: parseInt(cmdArray[5]),
			 y: parseInt(cmdArray[6]),
			 z: parseInt(cmdArray[4])},
			{x: parseInt(cmdArray[8]),
			 y: parseInt(cmdArray[9]),
			 z: parseInt(cmdArray[7])} );

	    console.log(">>" + cmdArray.toString() + "<<");

	    break;

	    // change to SHRDLU coordinates
	case "MOVE":
	    this.move({x: parseInt(cmdArray[2]),
		       y: parseInt(cmdArray[3]),
		       z: parseInt(cmdArray[1])});

	    console.log(">>" + cmdArray.toString() + "<<");

	    break;

	case "GRASP":
	    var targetName;

	    if (cmdArray.length > 1) {
		targetName = cmdArray[1];
	    } else {
		targetName = "";
	    }

	    this.grasp(targetName);

	    console.log(">>" + cmdArray.toString() + "<<");

	    break;

	case "RELEASE":
	    this.release();

	    console.log(">>" + cmdArray.toString() + "<<");

	    break;

	case "BLINK":
	    var targetName;

	    if (cmdArray.length > 1) {
		targetName = cmdArray[1];
	    } else {
		targetName = "";
	    }

	    this.blink(targetName);

	    console.log(">>" + cmdArray.toString() + "<<");

	    break;

	default:
	//    console.log('default');
	    break;

	}
    }


    function create(name, type, color, dim, pos) {

	var nobj = objs.addSC(name, type.toLowerCase(),
			      stdColors[color.toLowerCase()],
			      dim, pos);

	nobj = objs.set[-1 + objs.set.length];

	console.log("added " + nobj.name + " " + nobj.type);

	scene.add(nobj.obj);

    }

    function move(position) {
	console.log("MOVE HAND TO: " +
		    position.x + ", " + position.y + ", " + position.z);
	this.hand = position;

	if (this.objInHand)
	{
	    this.objInHand.moveto(position);
	}

	return null;
    }

    function grasp(targetName) {

	if (targetName) {

	    this.objInHand = objs.getByName(targetName);

	} else {

	    this.objInHand = objs.getClosest(this.hand);

	}

	console.log("grasp: " + this.objInHand.name);

	this.objInHand.pickup();

	return this.objInHand;
    }

    function blink(targetName) {

	var blinkobj;

	if (targetName) {

	    blinkobj = objs.getByName(targetName);

	} else {

	    blinkobj = objs.getClosest(this.hand);

	}

	if (blinkobj) {
	    console.log("blink: " + blinkobj.name);

	    blinkobj.blink();

	} else {
	    console.log("no such name");
	}

	return;
    }

    function release() {

	var target = objs.floor(this.hand);

	console.log("moving to:" + target.toString());
	console.log("this is what's moving: " + this.objInHand.name);

	this.objInHand.moveto({x: this.hand.x,
			       y: target + this.objInHand.dimension.y / 2,
			       z: this.hand.z});

	this.objInHand = null;

	return null;
    }

}


// The collection of objects that can be manipulated is organized by a
// blockCollection object.  The main part of it is the .set, an array
// of block objects.
function blockCollection() {
    this.set = [];

    this.add = add;
    this.addSC = addSC;

    this.getClosest = getClosest;
    this.getClosestSC = getClosestSC;
    this.getPoised = getPoised;
    this.getByName = getByName;

    this.floor = floor;

    // Create and add a new block object to the set.
    function add(name, type, color, dimension, position) {
	return this.set.push(new block(name, type, color, dimension, position));
    }

    // Create and add a new block object using SHRDLU coordinate system
    // where the control point is the front left corner of the object.
    function addSC(name, type, color, dimension, position) {
	var positionSC = {x: position.x + dimension.x / 2,
			  y: position.y,
			  z: position.z + dimension.z / 2};
	return this.add(name, type, color, dimension, positionSC);
    }

    // Returns the object closest to the input position.  There will
    // be circumstances where this is not correct, but I think they
    // will be uncommon.
    function getClosest(position)
    {
	var out = null;
	var min = 1.0e35;

	for (var i = 0; i < this.set.length; i++)
	{
	    var dist =
		Math.pow(position.x - this.set[i].obj.position.x, 2) +
		Math.pow(position.y - this.set[i].obj.position.y, 2) +
		Math.pow(position.z - this.set[i].obj.position.z, 2);

	    if (dist < min)
	    {
		out = this.set[i];
		min = dist;
	    }
	}

	return out;
    }

    // Returns the object closest to the input position in SHRDLU
    // coordinates.  Basically this only means that an object's
    // control point is its lower left corner.
    function getClosestSC(position)
    {
	var out = null;
	var min = 1.0e35;

	for (var i = 0; i < this.set.length; i++)
	{

	    var controlPt = this.set[i].controlPt();

	    var dist =
		Math.pow(position.x - controlPt.x, 2) +
		Math.pow(position.y - controlPt.y, 2) +
		Math.pow(position.z - controlPt.z, 2);

	    if (dist < min)
	    {
		out = this.set[i];
		min = dist;
	    }
	}

	return out;
    }


    // Returns the object that is up (i.e. poised to be put somewhere).
    function getPoised()
    {
	for (var i = 0; i < this.set.length; i++)
	{
	    if (this.set[i].upflag)
	    {
		return this.set[i];
	    }
	}
	return null;
    }

    // Returns the object of the given name.  Remove
    function getByName(targetName)
    {

	for (var i = 0; i < this.set.length; i++)
	{
	    if (this.set[i].name == targetName)
	    {
		return this.set[i];
	    }
	}
	return null;
    }

    // returns zero if no object is below the input position.  But if
    // there are any objects at this position, returns the height of
    // the highest object.
   function floor(position) {

	var out = 0;
	for (i = 0 ; i < this.set.length; i++)
	{

	    // console.log(this.set[i].name + ">>top>>" + 
	    // 		this.set[i].onTop(position).toString());
	    // console.log(JSON.stringify(position) + 
	    // 		JSON.stringify(this.set[i].obj.position));

	    out = Math.max(out, this.set[i].onTop(position));
	}

       return out;
    }
}


// A block object contains the graphical representation of some
// manipulable object in the scene.  this.obj is the actual graphical
// representation, while the rest is bookkeeping information and
// characteristics.
function block(name, type, color, dimension, position) {
    this.name = name;
    this.type = type;
    this.color = color;
    this.dimension = dimension;

    // Three.js defines the position of an object to be some point at
    // roughly the center of the object.  For this application, we
    // want the position of the middle of the bottom.  So the y
    // dimension is always adjusted by half the height of the object.

    this.render = render;
    this.moveto = moveto;
    this.pickup = pickup;
    this.putdown = putdown;
    this.putTarget = putTarget;

    this.blink = blink;

    this.onTop = onTop;
    this.top = top;

    // SHRDLU thinks an object's control point is the min(x), min(y), min(z).
    this.controlPt = controlPt;

    // Indicates whether the object is up in the sense of being ready
    // to be put down somewhere.  That is, an object can be "up"
    // because it's sitting on something else, or "up" because someone
    // is holding it.
    this.upflag = false;

    var geometry;

    switch (type)
    {
    case "pyramid":
	geometry =
	    new THREE.CylinderGeometry( 0,
					Math.sqrt(Math.pow(dimension.x/2,2) +
						  Math.pow(dimension.z/2,2)),
					dimension.y, 4, 4 );
	break;

    case "box":
	geometry = new THREE.BoxGeometry( dimension.x,
    					  dimension.y,
 					  dimension.z );

	break;

    default:
	geometry = new THREE.CubeGeometry( dimension.x,
    					   dimension.y,
 					   dimension.z );
	break;
    }

    var material = new THREE.MeshLambertMaterial(
	{ color: color,
	  shading: THREE.FlatShading,
	  overdraw: .5 } );

    // Create the graphical object, the important part of the block.
    this.obj = new THREE.Mesh( geometry, material );

    this.obj.scale.y = 1;

    // We want our pyramids to be square with our blocks, so rotate
    // them slightly.
    if (this.type == "pyramid") this.obj.rotation.y = Math.PI/4;

    this.obj.position.x = position.x;
    this.obj.position.y = position.y + (dimension.y / 2);
    this.obj.position.z = position.z;

    function render() {}

    // Returns the middle of the top surface of this object.
    function top()
    {
	if (this.type == "box")
	{
	    out = { x: this.obj.position.x,
		    y: this.obj.position.y - this.dimension.y / 2,
		    z: this.obj.position.z };
	} else {

	    out = { x: this.obj.position.x,
		    y: this.obj.position.y + this.dimension.y / 2,
		    z: this.obj.position.z };
	}

	return out;
    }

    // returns the height of the block if the x and z define a point
    // above it, zero otherwise.  If the input position is equal to
    // the object position, then the test is degenerate and will
    // return zero.
    function onTop(position)
    {
	var out;

	if (Math.sqrt(Math.pow(position.x - this.obj.position.x, 2) + 
		      Math.pow(position.y - this.obj.position.y, 2) +
		      Math.pow(position.z - this.obj.position.z, 2)) < 10) {
	    return 0;
	}

	if ((this.obj.position.x - (this.dimension.x / 2) < position.x) &&
	    (position.x < (this.obj.position.x + (this.dimension.x / 2))))
	{
	    if ((this.obj.position.z - (this.dimension.z / 2) < position.z) &&
		(position.z < (this.obj.position.z + (this.dimension.z / 2))))
	    {

		out = this.top().y; //obj.position.y + this.dimension.y / 2;
		return out;
	    }
	}

	return 0;
    }

    // Moves the object to a specified target point.
    function moveto( target )
    {
	console.log("moveto:" + target.x.toString() + "," +
		    target.y.toString() + "," +
		    target.z.toString());
	console.log("from:" + this.obj.position.toArray().toString());

	var tween = new TWEEN.Tween(this.obj.position).to(target, 2000)
	    .easing(TWEEN.Easing.Quartic.Out)
	    .start();
    }

    // Moves an object to a preset "pickupHeight".
    function pickup()
    {
	console.log("pickup: " + this.name);
	var target = { x: this.obj.position.x,
		       y: this.obj.position.y + pickupHeight,
		       z: this.obj.position.z };

	this.upflag = true;

	this.moveto(target);

    }

    // Moves an object to a specified point while also flipping the upflag.
    function putTarget(target)
    {
	console.log("putTarget: " + this.name);
	if (! this.upflag) return;

	this.moveto({x: target.x,
		     y: target.y + this.dimension.y / 2,
		     z: target.z});

	this.upflag = false;
    }

    // Subtracts the pickupHeight from an object's height.  Sort of a
    // crude way to put something down, really.
    function putdown()
    {
	console.log("putdown: " + this.name);
	var target = { x: this.obj.position.x,
		       y: this.obj.position.y - pickupHeight,
		       z: this.obj.position.z };

	this.moveto(target);

	this.upflag = false;
    }

    // Returns the SHRDLU control point for this object.
    function controlPt()
    {
	return {x: this.obj.position.x - this.dimension.x / 2,
		y: this.obj.position.y - this.dimension.y / 2,
		z: this.obj.position.z - this.dimension.z / 2 };
    }

    // currently a NOP.  Not sure what its purpose is in SHRDLU.
    function blink()
    {
	console.log(this.name + " is blinking!");

	return;
    }
}

// Sets everything up.
function init() {

    console.log("hello there");
    console.log(document.URL);

    container = document.createElement( 'div' );
    document.body.appendChild( container );

    var info = document.createElement( 'div' );
    info.style.position = 'absolute';
    info.style.top = '10px';
    info.style.width = '100%';
    info.style.textAlign = 'center';
    info.innerHTML = '';
    container.appendChild( info );

    // The clickable interface seems only to work with the perspective
    // camera.  Something to do with the operation of the raycaster.

    // camera = new THREE.OrthographicCamera( window.innerWidth / -2,
    // 					   window.innerWidth / 2,
    // 					   window.innerHeight / 2,
    // 					   window.innerHeight / -2,
    // 					   -500, 1000 );
     camera = new THREE.PerspectiveCamera(70,
					  window.innerWidth / window.innerHeight,
					  1, 10000 );

    camera.position.x = 300;
    camera.position.y = 300;
    camera.position.z = 1300;

    scene = new THREE.Scene();

    // Draw the grid that makes up the floor.
    var size = 1200, step = 50;

    var geometry = new THREE.Geometry();

    for ( var i = 0; i <= size; i += step ) {

	geometry.vertices.push( new THREE.Vector3(   0, 0, i ) );
	geometry.vertices.push( new THREE.Vector3(   size, 0, i ) );

	geometry.vertices.push( new THREE.Vector3( i, 0, 0 ) );
	geometry.vertices.push( new THREE.Vector3( i, 0,   size ) );

    }

    var material = new THREE.LineBasicMaterial( { color: 0x0000ff,
						  opacity: 0.2 } );

    var line = new THREE.Line( geometry, material );
    line.type = THREE.LinePieces;
    scene.add( line );

    // Add some axes
    var xaxisGeometry = new THREE.Geometry();
    xaxisGeometry.vertices.push(new THREE.Vector3(0, 0, 0));
    xaxisGeometry.vertices.push(new THREE.Vector3(100, 0, 0));
    var xaxisMaterial = new THREE.LineBasicMaterial( {color: 0xff0000 });
    var xaxis = new THREE.Line(xaxisGeometry, xaxisMaterial);
    xaxis.type = THREE.LinePieces;
    scene.add(xaxis);

    var yaxisGeometry = new THREE.Geometry();
    yaxisGeometry.vertices.push(new THREE.Vector3(0, 0, 0));
    yaxisGeometry.vertices.push(new THREE.Vector3(0, 100, 0));
    var yaxisMaterial = new THREE.LineBasicMaterial( {color: 0xff00ff });
    var yaxis = new THREE.Line(yaxisGeometry, yaxisMaterial);
    yaxis.type = THREE.LinePieces;
    scene.add(yaxis);

    var zaxisGeometry = new THREE.Geometry();
    zaxisGeometry.vertices.push(new THREE.Vector3(0, 0, 0));
    zaxisGeometry.vertices.push(new THREE.Vector3(0, 0, 100));
    var zaxisMaterial = new THREE.LineBasicMaterial( {color: 0x00ff00 });
    var zaxis = new THREE.Line(zaxisGeometry, zaxisMaterial);
    zaxis.type = THREE.LinePieces;
    scene.add(zaxis);


    // This is where we're adding objects.  This is just a set of
    // temporary hacks and the system should add its objects via the
    // CREATE command in cmds.parse.

    objs.addSC('B1', "block", 0xff0000,
     	       {x: 100, y: 100, z: 100}, {x: 110, y:0, z:100});

    objs.addSC('B2', "pyramid", 0x00ff00,
     	       {x: 100, y: 100, z: 100}, {x:110, y:100, z:100});

    objs.addSC('B3', "block", 0x00ff00,
      	       {x: 200, y: 200, z: 200}, {x:0, y:0, z:400});

    objs.addSC('B4', "pyramid", 0x0000ff,
      	       {x: 200, y:200, z:200}, {x:640, y:1, z:640});

    objs.addSC('B5', "pyramid", 0xff0000,
      	       {x: 100, y:300, z:100}, {x:100, y:200, z:500});

    objs.addSC('B6', "block", 0xff0000,
      	       {x: 300, y:300, z:200}, {x:300, y:0, z:0});

    objs.addSC('B7', "block", 0x00ff00,
      	       {x: 250, y:200, z:200}, {x:240, y:300, z:0});

    objs.addSC('B10', "block", 0x0000ff,
      	       {x: 100, y:400, z:200}, {x:640, y:0, z:300});

    // This has been enlarged 10 units on each side.  We cheat so that the
    // 3js rendering will look better.
    objs.addSC('BOX', "box", 0xffffff,
      	       {x: 396, y:100, z:396}, {x:590, y:0, z:590});

//     objs.add('cube1', "block", Math.random() * 0xffffff,
//                  {x: 50, y:50, z:50}, {x:0, y:0, z:0});

//    objs.add('cube2', "block", Math.random() * 0xffffff,
//                 {x: 50, y:50, z:50}, {x:100, y:0, z:100});

//    objs.add('cube3', "block", Math.random() * 0xffffff,
//                 {x: 75, y:75, z:75}, {x:-100, y:0, z:100});

//     objs.add('pyramid1', "pyramid", Math.random() * 0xffffff,
//                  {x: 50, y:50, z:50}, {x:100, y:0, z:-100});

//     objs.add('box1', 'box', Math.random() * 0xffffff,
//                  {x: 100, y:25, z:100}, {x:-100, y:0, z:-100});

    // Add all the objects to the scene.
    for (i = 0; i < objs.set.length; i++) {
	scene.add(objs.set[i].obj);
    }

    // Lights
    var ambientLight = new THREE.AmbientLight( 0x0c );
    scene.add( ambientLight );

    var directionalLight = new THREE.DirectionalLight( 0x888888 );
    directionalLight.position.x =  - 0.5;
    directionalLight.position.y =  0.5;
    directionalLight.position.z =  - 0.5;
    directionalLight.position.normalize();
    scene.add( directionalLight );

    var directionalLight = new THREE.DirectionalLight( 0xffffff );
    directionalLight.position.x =  0.5;
    directionalLight.position.y =  0.5;
    directionalLight.position.z =  0.5;
    directionalLight.position.normalize();
    scene.add( directionalLight );

    projector = new THREE.Projector();
    renderer = new THREE.CanvasRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );

    container.appendChild( renderer.domElement );

//    stats = new Stats();
//    stats.domElement.style.position = 'absolute';
//    stats.domElement.style.top = '0px';
//    container.appendChild( stats.domElement );

    //

    document.addEventListener( 'mousedown', onDocumentMouseDown, false );
    window.addEventListener( 'resize', onWindowResize, false );

    function onDocumentMouseDown( event ) {

	event.preventDefault();

	document.addEventListener('mousemove', onDocumentMouseMove, false);
	document.addEventListener('mouseup', onDocumentMouseUp, false);
	document.addEventListener('mouseout', onDocumentMouseOut, false);

	mouseXOnMouseDown = event.clientX - window.innerWidth / 2;
	targetRotationOnMouseDown = targetRotation;
    }

    function onDocumentMouseMove( event ) {

	mouseX = event.clientX - window.innerWidth / 2;

	targetRotation = targetRotationOnMouseDown +
	    2 * Math.PI * ( mouseX - mouseXOnMouseDown ) / window.innerWidth ;

    }

    function onDocumentMouseUp( event ) {

	document.removeEventListener( 'mousemove', onDocumentMouseMove, false );
	document.removeEventListener( 'mouseup', onDocumentMouseUp, false );
	document.removeEventListener( 'mouseout', onDocumentMouseOut, false );

    }

    function onDocumentMouseOut( event ) {

	document.removeEventListener( 'mousemove', onDocumentMouseMove, false );
	document.removeEventListener( 'mouseup', onDocumentMouseUp, false );
	document.removeEventListener( 'mouseout', onDocumentMouseOut, false );

    }
}

function onWindowResize() {

    camera.left = window.innerWidth / - 2;
    camera.right = window.innerWidth / 2;
    camera.top = window.innerHeight / 2;
    camera.bottom = window.innerHeight / - 2;

    camera.updateProjectionMatrix();

    renderer.setSize( window.innerWidth, window.innerHeight );

}

function animate() {

    requestAnimationFrame( animate );

    render();
//    stats.update();

    TWEEN.update();
}

function getSecond() { return Math.round(Date.now()/2000); }

var second = getSecond();
function render() {

    var timer = Date.now() * 0.0001;

    camera.position.x = 1000 * Math.cos( targetRotation );
    camera.position.y = 450;
    camera.position.z = 1000 * Math.sin( targetRotation );

    scene.position.x = 500; scene.position.z = 500;
    camera.lookAt( scene.position );


    renderer.render( scene, camera );

    var testSecond = getSecond();
    if (testSecond > second) {

	second = testSecond;

	var cmdUrl = "http://block.cs.brown.edu?actget=top";

	xmlHttp = new XMLHttpRequest();
	xmlHttp.onreadystatechange = processRequest;
	xmlHttp.open( "GET", cmdUrl, true );
	xmlHttp.send( null );

	var resUrl = "http://block.cs.brown.edu?resget=top";

	var xmlResHttp = new XMLHttpRequest();
	xmlResHttp.onreadystatechange = processResponse;
	xmlResHttp.open( "GET", resUrl, true );
	xmlResHttp.send( null );


    }

    function processResponse()
    {
	if ( xmlResHttp.readyState == 4 && xmlResHttp.status == 200 )
	{
	    msg = xmlResHttp.responseText;

	    if ( msg.match(/-empty-/g) ) return;

	    //console.log(msg);

	    var tdiv = document.getElementById("infoScroll");
	    tdiv.innerHTML = tdiv.innerHTML + '<p class="shrdlu-response">' + msg + "</p>";

	    $("#infoScroll").html(tdiv.innerHTML);

	    $('.ui-dialog').stop().animate({
		scrollTop: $("#infoBox")[0].scrollHeight}, 800);
        }
    }

    function processRequest()
    {
	if ( xmlHttp.readyState == 4 && xmlHttp.status == 200 )
	{
	    msg = xmlHttp.responseText;
	    // trim off EOL
            cmds.parse(msg.substring(0, msg.length - 1));
        }
    }
}


