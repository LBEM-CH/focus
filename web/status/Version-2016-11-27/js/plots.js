function weekendAreas(axes) {

    var markings = [];
    d = new Date(axes.xaxis.min);

    // go to the first Saturday

    d.setDate(d.getDate() - ((d.getDay() + 1) % 7));
    d.setSeconds(0);
    d.setMinutes(0);
    d.setHours(0);

    var i = d.getTime();

    // when we don't set yaxis, the rectangle automatically
    // extends to infinity upwards and downwards

    do {
        markings.push({ xaxis: { from: i, to: i + 2 * 24 * 60 * 60 * 1000 } });
        i += 7 * 24 * 60 * 60 * 1000;
    } while (i < axes.xaxis.max);

    return markings;
}

function getPlotOptions(type, minX, maxX, minY, maxY) {
    var options = {
        series: {
            points: {
                show: true
            }
        },
        grid: {
            hoverable: true, //IMPORTANT! this is needed for tooltip to work
            markings: weekendAreas
        },
        xaxis: { 
            mode: "time",
            timezone: "browser",
            tickLength: 5,
            min: minX,
            max: maxX
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
            content: type + " on '%s' at %x is %y",
            shifts: {
                x: -60,
                y: 25
            }
        }
    };

    return options;
}

function plotLastMonthLogData() {
    var lastMonth = new Date();
    lastMonth.setMonth(lastMonth.getMonth() - 1);
    plotLogData(Date.parse(lastMonth.toString()),-1);
}

function plotLastWeekLogData() {
    var lastWeek = new Date();
    lastWeek.setDate(lastWeek.getDate() - 7);
    plotLogData(Date.parse(lastWeek.toString()),-1);
}

function plotTodaysLogData() {
    var today = Date.parse(new Date().toDateString());
    plotLogData(today,-1);
}

function plotLast72hrsLogData() {
    var last72 = new Date();
    last72.setTime(last72.getTime() - 3 * 24 * 60 * 60 * 1000);
    plotLogData(Date.parse(last72.toString()),-1);
}

function plotLast24hrsLogData() {
    var last24 = new Date();
    last24.setTime(last24.getTime() - 24 * 60 * 60 * 1000);
    plotLogData(Date.parse(last24.toString()),-1);
}

function plotLast12hrsLogData() {
    var last12 = new Date();
    last12.setTime(last12.getTime() - 12 * 60 * 60 * 1000);
    plotLogData(Date.parse(last12.toString()),-1);
}

function plotLast03hrsLogData() {
    var last03 = new Date();
    last03.setTime(last03.getTime() -  3 * 60 * 60 * 1000);
    plotLogData(Date.parse(last03.toString()),-1);
}

function setupPlotSeclection(type, plotObj) {
    $("#log-"+type+"-plot").bind("plotselected", function (event, ranges) {

        // do the zooming
        $.each(plotObj.getXAxes(), function(_, axis) {
            var opts = axis.options;
            opts.min = ranges.xaxis.from;
            opts.max = ranges.xaxis.to;
        });
        plotObj.setupGrid();
        plotObj.draw();
        plotObj.clearSelection();
    });
}

function plotDataType(types, microscopes, minMSecs, maxMSecs, typeMaxVals) {
    var typeData = [];
    var minDataMSecs = maxMSecs; // Initialize with the highest value and then find the lowest
    for(t=0; t<types.length; t++) {
        var type = types[t];
        var allData = [];
        for(m=0; m<microscopes.length; m++) {
            var microscope = microscopes[m];
            var path = window.location.href.toString();
            var idx = path.lastIndexOf('/');
            var file = path.substr(0, idx) + "/../logs/" + microscope + "." + type

            var rawFile;
            if (window.XMLHttpRequest) {
                rawFile = new XMLHttpRequest();               
            } else {               
                rawFile = new ActiveXObject("Microsoft.XMLHTTP");               
            }

            rawFile.open("GET", file, false);
            rawFile.onreadystatechange = function () {
                if(rawFile.readyState === 4) {
                    if(rawFile.status === 200 || rawFile.status == 0) {
                        var allText = rawFile.responseText;
                        var allTextLines = allText.split("\n");
                        var ldata = [];
                        for(i=allTextLines.length-1; i>=0; i--) {
                            var line = allTextLines[i];
                            if(line != "") {
                                var cells = line.split("@@");
                                var msecs = Number(cells[0]);
                                if(msecs >= minMSecs && msecs <= maxMSecs) {
                                    ldata.push([msecs, cells[1]]);
                                    if(msecs < minDataMSecs) minDataMSecs = msecs;
                                }
                            }
                        }

                        allData.push({data: ldata, label: microscope});
                    }
                }
            }
            rawFile.send(null);
        }
        typeData.push(allData);
    }

    //Plot
    for(t=0; t< types.length; t++) {
        var type = types[t];
        var maxY = typeMaxVals[t];
        var data = typeData[t];
        var options = getPlotOptions(type, minDataMSecs, maxMSecs, 0.0, maxY);

        var plotObj = $.plot($("#log-"+type+"-plot"), data, options);

        //Do the following on selection
        setupPlotSeclection(type, plotObj);
    }
}

function plotLogData(minMSecs, maxMSecs) {
    var microscopes = [];
    if(document.getElementById("titan-check").checked == true) microscopes.push("titan");
    if(document.getElementById("polara-check").checked == true) microscopes.push("polara");
    if(document.getElementById("talos-check").checked == true) microscopes.push("talos");
    if(document.getElementById("cm200-check").checked == true) microscopes.push("cm200");

    if(minMSecs < 0) minMSecs = 0;
    if(maxMSecs < 0) maxMSecs = Date.parse(Date().toString());
    
    var types = ["mean", "resolution", "defocus", "drift"];
    var maxVals = [10.0, 20.0, 5.0, 100.0];
    plotDataType(types, microscopes, minMSecs, maxMSecs, maxVals);
};
