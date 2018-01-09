var canvas = document.getElementById('fractalCanvas');
var ctx = canvas.getContext('2d');

var intv = setInterval(update_fractal, 33);

(function() {
    ctx.canvas.width = window.innerWidth;
    ctx.canvas.height = window.innerHeight;
})();

function _arrayBufferToBase64( buffer ) {
    var binary = '';
    var bytes = new Uint8Array( buffer );
    var len = bytes.byteLength;
    for (var i = 0; i < len; i++) {
        binary += String.fromCharCode( bytes[ i ] );
    }
    return window.btoa( binary );
}

function update_fractal() {
    var oReq = new XMLHttpRequest();
    
    urlStr = "/fractal|" + canvas.width + "|" + canvas.height;
    
    oReq.open("GET", urlStr, true);
    oReq.responseType = "arraybuffer";

    oReq.onload = function (oEvent) {
        var arrayBuffer = oReq.response; // Note: not oReq.responseText
        if (arrayBuffer) {
            var byteArray = new Uint8ClampedArray(arrayBuffer);
            
            var imgData = ctx.getImageData(0, 0, canvas.width, canvas.height);
            var rawData = imgData.data;
            
            for(var i = 0; i < rawData.length; i++) {
                rawData[i] = byteArray[i];
            }
            ctx.putImageData(imgData, 0, 0);
        }
    };

    oReq.send(null);
}

function quitApp() {
    $.get( "/quit", function( data ) {
        $( "#debug" ).text( data );
    });
    clearInterval(intv);
}
