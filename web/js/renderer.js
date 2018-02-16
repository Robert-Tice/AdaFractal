var canvas = document.getElementById( 'fractalCanvas' );
var ctx = canvas.getContext( '2d' );

var fractal_type = "fixed_fractal"

function update_fractal() {
    var oReq = new XMLHttpRequest();

    urlStr = "/" + fractal_type;

    oReq.open( "GET", urlStr, true );
    oReq.responseType = "arraybuffer";

    oReq.onload = function ( oEvent ) {
        var arrayBuffer = oReq.response; // Note: not oReq.responseText
        if ( arrayBuffer ) {
            var byteArray = new Uint8ClampedArray(arrayBuffer);

            var imgData = ctx.getImageData(0, 0, canvas.width, canvas.height);
            var rawData = imgData.data;

            for (var i = 0; i < rawData.length; i++) {
                rawData[i] = byteArray[i];
            }
            
            $.get( "/compute_time", function ( data ) {
                if(fractal_type == "fixed_fractal")
                    $( "#fixed_speed" ).text( data + " ms" );
                else if(fractal_type == "float_fractal")
                    $( "#float_speed" ).text( data + " ms" );
            });

            ctx.putImageData(imgData, 0, 0);

            window.requestAnimationFrame(update_fractal);
        }
    };

    oReq.send( null );
}

function quitApp() {
    $.get( "/quit", function (data) {
        $( "#debug" ).text(data);
    });
    window.cancelAnimationFrame();
}

function changeSize(zm, mx, my) {
    width = Math.floor( $( "#canvas_container" ).width() );
    height = Math.floor( $( "#canvas_container" ).height() );

    ctx.canvas.width = width;
    ctx.canvas.height = height;
    
    if(mx == 0)
        mx = width / 2;
    if(my == 0)
        my = height / 2;

    var getStr = "/window|" + width + "|" + height + "|" + zm + "|" + mx + "|" + my;
    $.get(getStr, function () {

        })
        .fail(function () {
            alert( "Could not resize Ada canvas" );
        });
}

changeSize(0, 0, 0);

$( window ).resize(function () {
    changeSize(0, -1, -1);
});

$( '#fractalCanvas' ).mousewheel(function(event) {
    changeSize(event.deltaY, event.pageX, event.pageY);
});

$( '#fractalCanvas' ).click(function(event) {
    event.preventDefault();
    if(event.shiftKey)
        changeSize(-10, event.pageX, event.pageY);
    else
        changeSize(10, event.pageX, event.pageY);
});

$( '#fixed_mandlebrot' ).click(function() {
    fractal_type = "fixed_fractal";
    $( '#float_mandlebrot' ).removeClass( 'active' );
    $( '#fixed_mandlebrot' ).addClass( 'active' );
    changeSize(0, -1, -1);
});
                             
$( '#float_mandlebrot' ).click(function() {
    fractal_type = "float_fractal";
    $( '#fixed_mandlebrot' ).removeClass( 'active' );
    $( '#float_mandlebrot' ).addClass( 'active' );
    changeSize(0, -1, -1);
});

$( '#reset_zoom' ).click(function() {
    $.get( "/reset", function ( data ) {
        
    }).fail(function () {
            alert( "Could not reset Ada canvas" );
        });
});

window.requestAnimationFrame(update_fractal);
