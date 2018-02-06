var canvas = document.getElementById('fractalCanvas');
var ctx = canvas.getContext('2d');

var iMouseX, iMouseY = 1;
var bMouseDown = false;
var iZoomRadius = 150;
var iZoomPower = 10;

function clear() { // clear canvas function
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
}

function drawScene() { // main drawScene function

    var imageData = ctx.getImageData(0, 0, ctx.canvas.width, ctx.canvas.height);
    var newCanvas = $("<canvas>")
        .attr("width", imageData.width)
        .attr("height", imageData.height)[0];

    newCanvas.getContext("2d").putImageData(imageData, 0, 0);


    clear(); // clear canvas
    if (bMouseDown) { // drawing zoom area
        ctx.drawImage(newCanvas, 0 - iMouseX * (iZoomPower - 1), 0 - iMouseY * (iZoomPower - 1), ctx.canvas.width * iZoomPower, ctx.canvas.height * iZoomPower);
        ctx.globalCompositeOperation = 'destination-atop';
        var oGrd = ctx.createRadialGradient(iMouseX, iMouseY, 0, iMouseX, iMouseY, iZoomRadius);
        oGrd.addColorStop(0.8, "rgba(0, 0, 0, 1.0)");
        oGrd.addColorStop(1.0, "rgba(0, 0, 0, 0.1)");
        ctx.fillStyle = oGrd;
        ctx.beginPath();
        ctx.arc(iMouseX, iMouseY, iZoomRadius, 0, Math.PI*2, true);
        ctx.closePath();
        ctx.fill();
    }
    // draw source image
    ctx.drawImage(newCanvas, 0, 0, ctx.canvas.width, ctx.canvas.height);
}

function update_fractal() {
    var oReq = new XMLHttpRequest();

    urlStr = "/fractal";

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

            drawScene();

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

function changeSize() {
    width = $(window).width();
    height = $(window).height();

    ctx.canvas.width = width;
    ctx.canvas.height = height;

    var getStr = "/window|" + width + "|" + height;
    $.get(getStr, function () {

        })
        .fail(function () {
            alert("Could not resize Ada canvas");
        });
}

changeSize();

$(window).resize(function () {
    changeSize();
});

window.requestAnimationFrame(update_fractal);

$('#fractalCanvas').mousemove(function(e) { // mouse move handler
    var canvasOffset = $(canvas).offset();
    iMouseX = Math.floor(e.pageX - canvasOffset.left);
    iMouseY = Math.floor(e.pageY - canvasOffset.top);
});
$('#fractalCanvas').mousedown(function(e) { // binding mousedown event
    bMouseDown = true;
});
$('#fractalCanvas').mouseup(function(e) { // binding mouseup event
    bMouseDown = false;
});
