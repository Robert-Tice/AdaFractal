var canvas = document.getElementById('fractalCanvas');
var ctx = canvas.getContext('2d');

changeSize();

$(window).resize(function () {
    changeSize();
});

var intv = setInterval(update_fractal, 330);

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
        }
    };

    oReq.send(null);
}

function quitApp() {
    $.get("/quit", function (data) {
        $("#debug").text(data);
    });
    clearInterval(intv);
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