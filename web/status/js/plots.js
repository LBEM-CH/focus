var types = ["defocus", "resolution", "mean", "drift", "icyness", "ccvalue"];
var data = [];
var minMSecs = 0;
var maxMSecs = 0;

function weekendAreas(axes) {

    var markings = [];
    var d = new Date(axes.xaxis.min);

    // go to the first Saturday

    d.setDate(d.getDate() - ((d.getDay() + 1) % 7));
    d.setSeconds(0);
    d.setMinutes(0);
    d.setHours(0);

    var i = d.getTime();

    // when we don't set yaxis, the rectangle automatically
    // extends to infinity upwards and downwards

    do {
        markings.push({xaxis: {from: i, to: i + 2 * 24 * 60 * 60 * 1000}});
        i += 7 * 24 * 60 * 60 * 1000;
    } while (i < axes.xaxis.max);

    return markings;
}

function getPlotOptions(type, minY, maxY) {
    var options = {
        series: {
            points: {
                show: true,
                lineWidth: 1,
                radius: 2,
                fill: true, fillColor: "rgba(230, 230, 230, 0.5)"
            }
        },
        colors: ["#377eb8", "#4daf4a", "#e41a1c", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf"],
        grid: {
            hoverable: true, //IMPORTANT! this is needed for tooltip to work
            markings: weekendAreas
        },
        xaxis: {
            mode: "time",
            timezone: "browser",
            tickLength: 5,
            min: minMSecs,
            max: maxMSecs
        },
        yaxis: {
            min: minY,
            max: maxY
        },
        selection: {
            mode: "x"
        },
        tooltip: true,
        tooltipOpts: {
            content: capitalizeFirstLetter(type) + " on '%s' at %x is %y",
            shifts: {
                x: -60,
                y: 25
            }
        }
    };

    return options;
}

function setupPlotSelection(type, plotObj) {
    $("#log-" + type + "-plot").bind("plotselected", function (event, ranges) {

        // do the zooming
        $.each(plotObj.getXAxes(), function (_, axis) {
            var opts = axis.options;
            opts.min = ranges.xaxis.from;
            opts.max = ranges.xaxis.to;
        });
        plotObj.setupGrid();
        plotObj.draw();
        plotObj.clearSelection();
    });
}

function loadData(microscope, callBackFn) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var filename = path.substr(0, idx) + "/../logs/" + microscope + ".data";

    var xmlhttp;
    if (window.XMLHttpRequest) {
        xmlhttp = new XMLHttpRequest();
    } else {
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }

    xmlhttp.onreadystatechange = function () {
        if (this.readyState == 4 && this.status == 200) {
            callBackFn(this, microscope);
        }
    };

    xmlhttp.open("GET", filename, true);
    xmlhttp.send();
}

function plotData(xhttp, microscope) {
    var allText = xhttp.responseText;
    var allTextLines = allText.split("\n");
    var t, i, j;
    var ldata = [];
    for (t = 0; t < types.length; t++) {
        ldata.push([]);
    }

    for (i = allTextLines.length - 1; i >= 0; i--) {
        var line = allTextLines[i];
        if (line != "") {
            var cells = line.split(";");
            
            //add zeros to cells if required
            for(j=0; j<types.length+1-cells.length; j++) {
		cells.push("0.0");
	    } 
           
            var msecs = Number(cells[0]);
            if (msecs >= minMSecs && msecs <= maxMSecs) {
                for (t = 0; t < types.length; t++) {
                    ldata[t].push([msecs, Number(cells[t + 1])]);
                }
            }
        }
    }

    for (t = 0; t < types.length; t++) {
        data[t].push({data: ldata[t], label: capitalizeFirstLetter(microscope)});
        drawPlots();
    }

}

function drawPlots() {
    //Generate plots without data
    var typeMaxVals = [5.0, 15.0, 5.0, 100.0, 3.0];
    var t;
    for (t = 0; t < types.length; t++) {
        var type = types[t];
        var maxY = typeMaxVals[t];
        var ldata = data[t];
        var options = getPlotOptions(type, 0.0, maxY);
        var plotObj = $.plot($("#log-" + type + "-plot"), ldata, options);

        //Do the following on selection
        setupPlotSelection(type, plotObj);
    }
}

function findMinimum(microscopes) {
    var minDataMSecs = maxMSecs; // Initialize with the highest value and then find the lowest
    for (m = 0; m < microscopes.length; m++) {
        var microscope = microscopes[m];
        var path = window.location.href.toString();
        var idx = path.lastIndexOf('/');
        var file = path.substr(0, idx) + "/../logs/" + microscope + ".data";

        var rawFile;
        if (window.XMLHttpRequest) {
            rawFile = new XMLHttpRequest();
        } else {
            rawFile = new ActiveXObject("Microsoft.XMLHTTP");
        }

        rawFile.open("GET", file, false);
        rawFile.onreadystatechange = function () {
            if (rawFile.readyState === 4) {
                if (rawFile.status === 200) {
                    var allText = rawFile.responseText;
                    var allTextLines = allText.split("\n");
                    var i = allTextLines.length - 1;
                    while(allTextLines[i] == "") {
                        i--;
                    }
                    var msecs = Number(allTextLines[i].split(';')[0]);
                    if (msecs < minDataMSecs) minDataMSecs = msecs;
                }
            }
        };
        rawFile.send(null);
    }

    return minDataMSecs;
}

function plotLogData(min, max, microscopes) {
    maxMSecs = max;
    if (maxMSecs < 0) {
        maxMSecs = Date.parse(Date().toString());
    }

    minMSecs = min;
    if (minMSecs < 0) {
        minMSecs = findMinimum(microscopes);
    }

    document.getElementById("plot-start-date").innerHTML = getDateString(new Date(minMSecs));
    document.getElementById("plot-end-date").innerHTML = getDateString(new Date(maxMSecs));

    data = [];
    var t;
    for (t = 0; t < types.length; t++) {
        data.push([]);
    }

    var i;
    for (i = 0; i < microscopes.length; i++) {
        loadData(microscopes[i], plotData);
    }
}
