// needed commands:
//   create a block/pyramid/box/whatever (at some place)
//   pick up a block or whatever
//   put it down
//   move hand


var container, stats;
var camera, scene, renderer, projector;
var objs = new blockCollection();
var cmds = new commands();

var pickupHeight = 125;

init();
animate();

// This object interprets text commands into object motion commands.
// The text commands are to be issued by a SHRDLU port and are in the
// SHRDLU coordinates (where z is up).
function commands() {
    // commands are these:
    //  CREATE/NAME/TYPE/COLOR/DIMX/DIMY/DIMZ/POSX/POSY/POSZ
    //  MOVE/POSX/POSY/POSZ
    //  GRASP
    //  RELEASE

    this.hand = {x: 0, y: 200, z: 0};

    this.parse = parse;

    this.create = create;
    this.move = move;
    this.grasp = grasp;
    this.release = release;

    this.objInHand = null;

    function parse(string)
    {
	cmdArray = string.split("/");

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



function blockCollection() {
    this.set = [];
    this.current = -1;

    this.add = add;
    this.getCurrent = getCurrent;

    this.getClosest = getClosest;
    this.getPoised = getPoised;

    this.floor = floor;

    function add(name, type, color, dimension, position) {
	this.set.push(new block(name, type, color, dimension, position));
    }


    // Returns the object closest to the input position.
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


    // returns an object and advances to the next one.
    function getCurrent() {
	if (this.set.length < 1)
	{
	    console.log("uh-oh");
	    return null;
	}
	else
	{
	    this.current += 1;

	    if (this.current >= this.set.length)
	    {
		this.current = 0;
	    }

	    return this.set[this.current];
	}
    }

    // returns zero if no object is at position, otherwise the height
    // of whatever is there.
   function floor(position) {

	var out = 0;
	for (i = 0 ; i < this.set.length; i++)
	{
	    out = Math.max(out, this.set[i].onTop(position));
	}

       return out;
    }
}


function block(name, type, color, dimension, position) {
    this.name = name;
    this.type = type;
    this.color = color;
    this.dimension = dimension;
    // we want the position of the middle of the bottom while three.js seems
    // to use the middle of the object.  So the y dimension is always adjusted by
    // half the height of the object.

    this.render = render;
    this.moveto = moveto;
    this.pickup = pickup;
    this.putdown = putdown;
    this.putTarget = putTarget;

    this.onTop = onTop;
    this.top = top;

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

    this.obj = new THREE.Mesh( geometry, material );

    this.obj.scale.y = 1;

    if (this.type == "pyramid") this.obj.rotation.y = Math.PI/4;

    console.log(position.x);

    this.obj.position.x = position.x;
    this.obj.position.y = position.y + (dimension.y / 2);
    this.obj.position.z = position.z;

    function render() {}

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

    // returns the height of the block if the x and z are on top of
    // it, zero otherwise.  If the input position is equal to the
    // object position, then the test is degenerate and will return
    // zero.
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

    function pickup()
    {
	console.log("pickup: " + this.name);
	var target = { x: this.obj.position.x,
		       y: this.obj.position.y + pickupHeight,
		       z: this.obj.position.z };

	this.upflag = true;

	this.moveto(target);

    }

    function putTarget(target)
    {
	console.log("putTarget: " + this.name);
	if (! this.upflag) return;

	this.moveto({x: target.x,
		     y: target.y + this.dimension.y / 2,
		     z: target.z});

	this.upflag = false;
    }


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


function init() {

    console.log("hello there");

    container = document.createElement( 'div' );
    document.body.appendChild( container );

    var info = document.createElement( 'div' );
    info.style.position = 'absolute';
    info.style.top = '10px';
    info.style.width = '100%';
    info.style.textAlign = 'center';
    info.innerHTML = '';
    container.appendChild( info );

    camera = new THREE.OrthographicCamera( window.innerWidth / -2,
					   window.innerWidth / 2,
					   window.innerHeight / 2,
					   window.innerHeight / -2,
					   -500, 1000 );
     camera = new THREE.PerspectiveCamera( 70, window.innerWidth / window.innerHeight, 1, 10000 );

   camera.position.x = 300;
    camera.position.y = 300;
    camera.position.z = 400;

    scene = new THREE.Scene();

    // Grid

    var size = 500, step = 25;

    var geometry = new THREE.Geometry();

    for ( var i = - size; i <= size; i += step ) {

	geometry.vertices.push( new THREE.Vector3( - size, 0, i ) );
	geometry.vertices.push( new THREE.Vector3(   size, 0, i ) );

	geometry.vertices.push( new THREE.Vector3( i, 0, - size ) );
	geometry.vertices.push( new THREE.Vector3( i, 0,   size ) );

    }

    var material = new THREE.LineBasicMaterial( { color: 0x0000ff, opacity: 0.2 } );

    var line = new THREE.Line( geometry, material );
    line.type = THREE.LinePieces;
    scene.add( line );

    // Axes
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
    var yaxisMaterial = new THREE.LineBasicMaterial( {color: 0xff0000 });
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

    // Cubes

    objs.add('cube1', "block", Math.random() * 0xffffff,
		  {x: 50, y:50, z:50}, {x:0, y:0, z:0});

    objs.add('cube2', "block", Math.random() * 0xffffff,
     		  {x: 50, y:50, z:50}, {x:100, y:0, z:100});

    objs.add('cube3', "block", Math.random() * 0xffffff,
     		  {x: 75, y:75, z:75}, {x:-100, y:0, z:100});

    objs.add('pyramid1', "pyramid", Math.random() * 0xffffff,
     		  {x: 50, y:50, z:50}, {x:100, y:0, z:-100});

    objs.add('box1', 'box', Math.random() * 0xffffff,
     		  {x: 100, y:25, z:100}, {x:-100, y:0, z:-100});

    for (i = 0; i < objs.set.length; i++) {

	console.log("i: " + i);

	console.log(objs.set[i].name);

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

//




// function onDocumentMouseDown( event ) {

//     event.preventDefault();

//     var vector = new THREE.Vector3(
// 	( event.clientX / window.innerWidth ) * 2 - 1,
// 	    - ( event.clientY / window.innerHeight ) * 2 + 1,
// 	0.5 );
//     projector.unprojectVector( vector, camera );

//     var raycaster = new THREE.Raycaster(
// 	camera.position,
// 	vector.sub( camera.position ).normalize() );

//     var intersects = raycaster.intersectObjects( scene.children );

//     console.log(intersects[0]);

//     if ( intersects.length > 0 ) {

// 	new TWEEN.Tween( intersects[ 0 ].object.position )
// 	    .to( {
// 		x: intersects[0].object.position.x,
// 		y: Math.random() * 125,
// 		z: intersects[0].object.position.z }, 2000 )
// 	    .easing( TWEEN.Easing.Elastic.Out).start();

/*	new TWEEN.Tween( intersects[ 0 ].object.rotation ).to( {
	    x: Math.random() * 2 * Math.PI,
	    y: Math.random() * 2 * Math.PI,
	    z: Math.random() * 2 * Math.PI }, 2000 )
	    .easing( TWEEN.Easing.Elastic.Out).start(); */

//     }

// }



function animate() {

    requestAnimationFrame( animate );

    render();
    stats.update();

    TWEEN.update();
}

function render() {

    var timer = Date.now() * 0.0001;

    camera.position.x = Math.cos( timer ) * 200;
    camera.position.z = Math.sin( timer ) * 200;
    camera.lookAt( scene.position );

    renderer.render( scene, camera );

}
