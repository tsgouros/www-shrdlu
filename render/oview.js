// To do list:

// Change so that coordinates refer to a corner, as in shrdlu
// Implement create object command,
// Have shrdlu populate the screen
// connect mover.lisp

var container, stats;
var camera, scene, renderer, projector;
var objs = new blockCollection();
var cmds = new commands();

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

    // Parses a command string into a method and its arguments, and
    // invokes the methods on the arguments.
    function parse(string)
    {
	cmdArray = string.split("/");

	console.log(">>" + cmdArray.toString() + "<<");

	switch ( cmdArray[0] )
	{
	case "CREATE":
	    this.create(cmdArray);
	    break;

	case "MOVE":
	    this.move({x: parseInt(cmdArray[1]),
		       y: parseInt(cmdArray[2]),
		       z: parseInt(cmdArray[3])});
	    break;

	case "GRASP":
	    this.grasp();
	    break;

	case "RELEASE":
	    this.release();
	    break;

	default:
	    console.log('default');
	    break;

	}
    }


    function create(cmdArray) {
	return null;
    }

    function move(position) {
	console.log("MOVE HAND TO: " + position.toString());
	this.hand = position;

	if (this.objInHand)
	{
	    this.objInHand.moveto(position);
	}

	return null;
    }

    function grasp() {

	this.objInHand = objs.getClosest(this.hand);

	console.log("grasp: " + this.objInHand.name);

	return this.objInHand;
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
    this.getPoised = getPoised;

    this.floor = floor;

    // Create and add a new block object to the set.
    function add(name, type, color, dimension, position) {
	this.set.push(new block(name, type, color, dimension, position));
    }

    // Create and add a new block object using SHRDLU coordinate system
    // where the control point is the front left corner of the object.
    function addSC(name, type, color, dimension, position) {
	var positionSC = {x: position.x + dimension.x / 2,
			  y: position.y,
			  z: position.z + dimension.z / 2};
	this.add(name, type, color, dimension, positionSC);
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


    // returns zero if no object is below the input position.  But if
    // there are any objects at this position, returns the height of
    // the highest object.
   function floor(position) {

	var out = 0;
	for (i = 0 ; i < this.set.length; i++)
	{
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

    this.onTop = onTop;
    this.top = top;

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

    console.log(position.x);

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

	if ((position.x == this.obj.position.x) &&
	    (position.y == this.obj.position.y) &&
	    (position.z == this.obj.position.z)) {
	    return 0;
	}

	if ((this.obj.position.x - (this.dimension.x / 2) < position.x) &&
	    (position.x < (this.obj.position.x + (this.dimension.x / 2))))
	{
	    if ((this.obj.position.z - (this.dimension.z / 2) < position.z) &&
		(position.z < (this.obj.position.z + (this.dimension.z / 2))))
	    {
		out = this.obj.position.y + this.dimension.y / 2;
		console.log("here:" + out);
		return out;
	    }
	}
	console.log("zero");
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

    objs.addSC('BOX', "box", 0xeeeeee,
      		  {x: 376, y:100, z:376}, {x:600, y:0, z:600});

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

    stats = new Stats();
    stats.domElement.style.position = 'absolute';
    stats.domElement.style.top = '0px';
    container.appendChild( stats.domElement );

    //

    document.addEventListener( 'mousedown', onDocumentMouseDown, false );
    window.addEventListener( 'resize', onWindowResize, false );

    function onDocumentMouseDown( event ) {

	event.preventDefault();

	var vector = new THREE.Vector3(
 	    ( event.clientX / window.innerWidth ) * 2 - 1,
 		- ( event.clientY / window.innerHeight ) * 2 + 1,
 	    0.5 );

	projector.unprojectVector( vector, camera );

	var raycaster = new THREE.Raycaster(
	    camera.position,
 	    vector.sub( camera.position ).normalize() );

	var intersects = raycaster.intersectObjects( scene.children );

	// Check was an object clicked?
	if (intersects.length > 0)
	{
	    var clicked = objs.getClosest(intersects[0].point);

	    console.log("clicked: " + clicked.name);

	    // If so, is there an object poised?

	    var poised = objs.getPoised();

	    if (poised)
	    {
		// Did we click on an object in the air?
		if (poised == clicked)
		{
		    poised.putdown();

		} else {

		    // If so, move the poised object onto the target object.
		    poised.putTarget(clicked.top());
		}

	    } else {

		// If not, poise the clicked object.

		if (clicked.upflag) {
		    clicked.putdown();
		} else {
		    clicked.pickup();
		}
	    }
	}
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
    stats.update();

    TWEEN.update();
}

var second = Math.round(Date.now()/5000);
function render() {

    var timer = Date.now() * 0.0001;

    camera.position.x = -800; //Math.cos( timer ) * 300;
    camera.position.y = 450;
    camera.position.z = 1000; //Math.sin( timer ) * 300;

    scene.position.x = 500; scene.position.z = 500;
    camera.lookAt( scene.position );


    renderer.render( scene, camera );

    if (Math.round(Date.now()/5000) > second) {

	second = Math.round(Date.now()/5000);

	var pop_url = "http://localhost:1337?actget=top";

	xmlHttp = new XMLHttpRequest();
	xmlHttp.onreadystatechange = ProcessRequest;
	xmlHttp.open( "GET", pop_url, true );
	xmlHttp.send( null );

    }

    function ProcessRequest()
    {
	if ( xmlHttp.readyState == 4 && xmlHttp.status == 200 )
	{
	    msg = xmlHttp.responseText;
	    // trim off EOL
            cmds.parse(msg.substring(0, msg.length - 1));
        }
    }
}


