var types = ["defocus", "resolution", "mean", "drift", "iciness", "ccvalue", "astig", "phase_shift"];
var typeMaxVals = [5.0, 8.0, 2.0, 100.0, 3.0, 0.3, 0.04, 180.0];
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
            markings: weekendAreas,
            backgroundColor: "rgba(210,210,210,0.3)",
            borderWidth: 0
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
        tooltip: true,
        tooltipOpts: {
            content: capitalizeFirstLetter(type) + " at %x was %y",
            shifts: {
                x: -60,
                y: 25
            }
        }
    };

    return options;
}

function setupPlotSelection(event, ranges, plotObj) {
    // do the zooming
    $.each(plotObj.getXAxes(), function (_, axis) {
        var opts = axis.options;
        opts.min = ranges.xaxis.from;
        opts.max = ranges.xaxis.to;
    });
    plotObj.setupGrid();
    plotObj.draw();
    plotObj.clearSelection();
}

function loadData(microscope, callBackFn) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var filename = path.substr(0, idx) + "/logs/" + microscope + ".data";

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
    var newData = [];
    var dateMap = {};
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
                var dt = new Date(0); // The 0 there is the key, which sets the date to the epoch
                dt.setUTCSeconds(msecs/1000);
                dt.setMinutes(0);
                dt.setSeconds(0);

                var newElement;
                if(dateMap[dt]){
                    newElement = dateMap[dt];
                } else {
                    newElement = [dt.getTime(), 0];
                    dateMap[dt] = newElement;
                    newData.push(newElement);
                }
                newElement[1] += 1;

                for (t = 0; t < types.length; t++) {
                    ldata[t].push([msecs, Number(cells[t + 1])]);
                }
            }
        }
    }

    var plotobjects = [];
    var plotObj;
    for (t = 0; t < types.length; t++) {
        var type = types[t];
        var maxY = typeMaxVals[t];
        var options = getPlotOptions(type, 0.0, maxY);
        plotObj = $.plot($("#log-" + type + "-plot"), [{data: ldata[t]}], options);
        plotobjects.push(plotObj);
    }

    var allPlotObject = $.plot($("#log-time-plot"), [{data: newData}], {
        series: {
            lines: {
                show: true,
                fill: true
            }
        },
        xaxis: {
            mode: "time",
            timezone: "browser",
            tickLength: 5,
            min: minMSecs,
            max: maxMSecs
        },
        yaxis: {
            min: 0
        },
        grid: {
            hoverable: true,
            markings: weekendAreas,
            backgroundColor: "rgba(210,210,210,0.5)",
            borderWidth: 0
        },
        legend: {
            show: false
        },
        selection: {
            mode: "x"
        },
        tooltip: true,
        tooltipOpts: {
            content: "%y images were recorded in an hour after %x"
        }
    });

    //Do the following on selection
    $("#log-time-plot").bind("plotselected", function (event, ranges) {

        //Change dates
        document.getElementById("plot-start-date").innerHTML = getDateString(new Date(ranges.xaxis.from));
        document.getElementById("plot-end-date").innerHTML = getDateString(new Date(ranges.xaxis.to));

        // do the zooming
        setupPlotSelection(event, ranges, allPlotObject);
        var plt;
        for (plt = 0; plt < plotobjects.length; plt++) {
            setupPlotSelection(event, ranges, plotobjects[plt]);
        }   
    });
}

function findMinimum(microscope) {
    var minDataMSecs = maxMSecs; // Initialize with the highest value and then find the lowest

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

    return minDataMSecs;
}

function plotLogData(min, max, microscope) {
    maxMSecs = max;
    if (maxMSecs < 0) {
        maxMSecs = Date.parse(Date().toString());
    }

    minMSecs = min;
    if (minMSecs < 0) {
        minMSecs = findMinimum(microscope);
    }

    document.getElementById("plot-start-date").innerHTML = getDateString(new Date(minMSecs));
    document.getElementById("plot-end-date").innerHTML = getDateString(new Date(maxMSecs));

    loadData(microscope, plotData);
}
