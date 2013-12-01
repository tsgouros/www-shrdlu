var container, stats;
var camera, scene, projector, renderer;

init();
animate();


function block(name, color, dimension, position) {
    this.name = name;
    this.color = color;
    this.dimension = dimension;
    this.position = position;

    this.geometry = new THREE.CubeGeometry( dimension.x, dimension.y, dimension.z );

    this.object = new THREE.Mesh( geometry, new THREE.MeshBasicMaterial( { color: this.color, opacity: 0.5 } ) );
    this.object.position.x = position.x;
    this.object.position.y = position.y;
    this.object.position.z = position.z;

    return this;
}


function init() {

    container = document.createElement( 'div' );
    document.body.appendChild( container );

    var info = document.createElement( 'div' );
    info.style.position = 'absolute';
    info.style.top = '10px';
    info.style.width = '100%';
    info.style.textAlign = 'center';
    info.innerHTML = '<a href="http://threejs.org" target="_blank">three.js</a> - clickable objects';
    container.appendChild( info );

    camera = new THREE.PerspectiveCamera( 70, window.innerWidth / window.innerHeight, 1, 10000 );
    camera.position.y = 300;
    camera.position.z = 500;

    scene = new THREE.Scene();

    for ( var i = 0; i < 20; i ++ ) {

	var b = block("b",
		      Math.random() * 0xffffff,
		      {x: 100, y: 100, z: 100},
		      {x: Math.random() *800 - 400, y: 0, z: Math.random() * 800 - 400});

	scene.add( b.object );

    }

    projector = new THREE.Projector();

    renderer = new THREE.CanvasRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );

    container.appendChild(renderer.domElement);

    stats = new Stats();
    stats.domElement.style.position = 'absolute';
    stats.domElement.style.top = '0px';
    container.appendChild( stats.domElement );

    document.addEventListener( 'mousedown', onDocumentMouseDown, false );

    //

    window.addEventListener( 'resize', onWindowResize, false );

}

function onWindowResize() {

    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();

    renderer.setSize( window.innerWidth, window.innerHeight );

}

function onDocumentMouseDown( event ) {

    event.preventDefault();

    var vector = new THREE.Vector3( ( event.clientX / window.innerWidth ) * 2 - 1, - ( event.clientY / window.innerHeight ) * 2 + 1, 0.5 );
    projector.unprojectVector( vector, camera );

    var raycaster = new THREE.Raycaster( camera.position, vector.sub( camera.position ).normalize() );

    var intersects = raycaster.intersectObjects( scene.children );

    if ( intersects.length > 0 ) {

	new TWEEN.Tween( intersects[ 0 ].object.position ).to( {
	    x: intersects[0].object.position.x,
	    y: Math.random() * 800 - 400,
	    z: intersects[0].object.position.z }, 2000 )
	    .easing( TWEEN.Easing.Elastic.Out).start();

    }

    /*
    // Parse all the faces
    for ( var i in intersects ) {

    intersects[ i ].face.material[ 0 ].color.setHex( Math.random() * 0xffffff | 0x80000000 );

    }
    */
}

//

function animate() {

    requestAnimationFrame( animate );

    render();
    stats.update();

}

var radius = 600;
var theta = 0;

function render() {

    TWEEN.update();

    theta += 0.1;

    camera.position.x = radius * Math.sin( THREE.Math.degToRad( theta ) );
    camera.position.y = radius ;//* Math.sin( THREE.Math.degToRad( theta ) );
    camera.position.z = radius * Math.cos( THREE.Math.degToRad( theta ) );
    camera.lookAt( scene.position );

    renderer.render( scene, camera );

}

