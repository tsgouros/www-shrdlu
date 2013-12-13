$(function() 
{
    $("#infoBox")
	.css( 
	    {
		"background":"rgba(255,255,255,0.5)"
	    })
	.dialog({ autoOpen: false, 
		  show: { effect: 'fade', duration: 500 },
		  hide: { effect: 'fade', duration: 500 },
		  position: {
		      my: "right bottom",
		      at: "right bottom",
		      of: window
		  }
		});
	
    $("#infoButton")
	.text("") // sets text to empty
	.css(
	    { "z-index":"2",
	      "background":"rgba(0,0,0,0)", "opacity":"0.9", 
	      "position":"absolute", "bottom":"4px", "left":"4px"
	    }) // adds CSS
	.append("<img width='36' height='35' src='import/icon-shrdlu.png'/>")
	.button()
	.click( 
	    function() 
	    { 
		$("#infoBox").dialog("open");
	    });

    $("#sentenceForm")
	.submit(function() {
	    var tmp = document.getElementById("cmdbox").value;
	    document.getElementById("cmdbox").value = "";

//	    var ralph = "hello"; //this.serialize();

	    var pushURL = "http://localhost:1337?cmd=" + 
		encodeURIComponent(tmp);

	    var xmlHttp = new XMLHttpRequest();
	    xmlHttp.onreadystatechange = function() {};
	    xmlHttp.open( "GET", pushURL, true );
	    xmlHttp.send( null );

	    var tdiv = document.getElementById("infoScroll");
	    tdiv.innerHTML = tdiv.innerHTML + '<p class="shrdlu-sentence">' + tmp + "</p>";
	    $("#infoScroll").html(tdiv.innerHTML);

	//    $("#infoBox").scrollTop($("#infoBox")[0].scrollHeight);
	//    console.log($("#infoBox")[0].scrollHeight);


	    $('.ui-dialog').stop().animate({
	     	scrollTop: $("#infoBox")[0].scrollHeight
	    }, 800);


	    // We don't want the display to be replaced by the return value,
	    // so return false.
	    return false;
	});

});