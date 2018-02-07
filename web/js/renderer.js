var canvas = document.getElementById('fractalCanvas');
var ctx = canvas.getContext('2d');

var zoom = 10;

var zoomMax = 1000;
var zoomMin = 10;

var fractal_type = "fixed_fractal"

function update_fractal() {
    var oReq = new XMLHttpRequest();

    urlStr = "/" + fractal_type;

    oReq.open("GET", urlStr, true);
    oReq.responseType = "arraybuffer";

    oReq.onload = function (oEvent) {
        var arrayBuffer = oReq.response; // Note: not oReq.responseText
        if (arrayBuffer) {
            var byteArray = new Uint8ClampedArray(arrayBuffer);

            var imgData = ctx.getImageData(0, 0, canvas.width, canvas.height);
            var rawData = imgData.data;

            for (var i = 0; i < rawData.length; i++) {
                rawData[i] = byteArray[i];
            }

            ctx.putImageData(imgData, 0, 0);

            window.requestAnimationFrame(update_fractal);
        }
    };

    oReq.send(null);
}

function quitApp() {
    $.get("/quit", function (data) {
        $("#debug").text(data);
    });
    window.cancelAnimationFrame();
}

function changeSize(zm, mx, my) {
    width = Math.floor( $(window).width() );
    height = Math.floor( $(window).height() );

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
            alert("Could not resize Ada canvas");
        });
}

function change_zoom(amount) {
    zoom += amount;

    if(zoom < zoomMin)
        zoom = zoomMin;
    else if(zoom > zoomMax)
        zoom = zoomMax;
}

changeSize(zoom, 0, 0);

$(window).resize(function () {
    zoom = 1;
    changeSize(zoom, 0, 0);
});

$('#fractalCanvas').mousewheel(function(event) {
    change_zoom(event.deltaY);
    changeSize(zoom, event.pageX, event.pageY);
});

$('#fractalCanvas').click(function(event) {
    if(event.shiftKey)
        change_zoom(-20);
    else
        change_zoom(20);
    event.preventDefault();
    changeSize(zoom, event.pageX, event.pageY);
});

$('#fixed_mandlebrot').click(function() {
    fractal_type = "fixed_fractal";
    $('#float_mandlebrot').removeClass('active');
    $('#fixed_mandlebrot').addClass('active');
});
                             
$('#float_mandlebrot').click(function() {
    fractal_type = "float_fractal";
    $('#fixed_mandlebrot').removeClass('active');
    $('#float_mandlebrot').addClass('active');
});

window.requestAnimationFrame(update_fractal);
