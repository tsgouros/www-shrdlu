// needed commands: 
//   create a block/pyramid/box/whatever (at some place)
//   pick up a block or whatever
//   put it down
//   move hand

var container, stats;
var camera, scene, renderer, projector;
var objs = new blockCollection();

init();
animate();

function blockCollection() {
    this.set = {};
    this.current = -1;
    this.nobjs = 0;

    this.add = add;
    this.getCurrent = getCurrent;
    this.getKeys = getKeys;
    this.getNth = getNth;


    function add(name, color, dimension, position) {
	this.set[name] = new block(name, color, dimension, position);
	this.nobjs += 1;
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

	    if (this.current >= Object.keys(this.set).length)
	    {
		this.current = 0;
	    }

	    return this.set[Object.keys(this.set)[this.current]];
	}
    }

    function getKeys() {
	return Object.keys(this.set);
    }

    function getNth(nth) {
	return this.set[this.getKeys()[nth]];
    }

    // returns zero if no object is at position, otherwise the height
    // of whatever is there.
    function floor(position) {

	var out = 0;
	for (i = 0 ; i < nobjs; i++) 
	{
	    out = Math.max(out, this.getNth(i).onTop(position));
	}
    }
}


function block(name, color, dimension, position) {
    this.name = name;
    this.color = color;
    this.dimension = dimension;
    this.position = position;

    this.render = render;
    this.moveto = moveto;
    this.pickup = pickup;
    this.putdown = putdown;

    this.onTop = onTop;

    this.upflag = false;

    var geometry = new THREE.CubeGeometry( dimension.x, 
					   dimension.y, 
					   dimension.z );

    var material = new THREE.MeshLambertMaterial( 
	{ color: color, 
	  shading: THREE.FlatShading, 
	  overdraw: .5 } );

    this.obj = new THREE.Mesh( geometry, material );

    this.obj.scale.y = 1;

    this.obj.position.x = position.x;
    this.obj.position.y = position.y;
    this.obj.position.z = position.z;

    function render() {}

    // returns the height of the block if the x and z are on top of
    // it, zero otherwise.
    function onTop(position) {
	if ((this.obj.position.x - (this.obj.dimension.x / 2) < position.x) &&
	    (position.x < (this.obj.position.x + (this.obj.dimension.x / 2))))
	{
	    if ((this.obj.position.z - (this.obj.dimension.z / 2) < position.z) &&
		(position.z < (this.obj.position.z + (this.obj.dimension.z / 2))))
	    {
		return this.obj.position.y + (this.obj.dimension.y / 2);
	    }
	}
	return 0;
    }

    function moveto( target ) {

	var tween = new TWEEN.Tween(this.obj.position).to(target, 2000);

	tween.start();
    }

    function pickup() {

	var target = { x: this.obj.position.x, 
		       y: this.obj.position.y + 125, 
		       z: this.obj.position.z };

	this.upflag = true;

	this.moveto(target);
	
    }

    function putdown() {
	var target = { x: this.obj.position.x, 
		       y: this.obj.position.y - 125, 
		       z: this.obj.position.z };

	this.upflag = false;

	this.moveto(target);
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
    camera.position.x = 200;
    camera.position.y = 100;
    camera.position.z = 200;

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

    var material = new THREE.LineBasicMaterial( { color: 0xff0000, opacity: 0.2 } );

    var line = new THREE.Line( geometry, material );
    line.type = THREE.LinePieces;
    scene.add( line );

    // Cubes

    objs.add('cube1', Math.random() * 0xffffff, 
		  {x: 50, y:50, z:50}, {x:0, y:25, z:0});

    objs.add('cube2', Math.random() * 0xffffff, 
		  {x: 50, y:50, z:50}, {x:100, y:25, z:100});

    objs.add('cube3', Math.random() * 0xffffff, 
		  {x: 75, y:75, z:75}, {x:-100, y:37, z:100});

    for (i = 0; i < objs.nobjs; i++) {

	console.log(objs.getNth(i).name);

	scene.add(objs.getNth(i).obj);
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

    var obj = objs.getCurrent();

    if (obj.upflag) {
	obj.putdown(); 
    } else {
	obj.pickup();
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
