function createLogTable(microscope) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var file = path.substr(0, idx) + "/../logs/" + microscope + ".log"

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
                var table = document.getElementById("log-table");
                table.innerHTML = "";
                for(i=0; i<allTextLines.length; i++) {
                    var line = allTextLines[i];
                    if(line != "") {
                        var dateAndLog = line.split("@@");
                        var date = getDateString(getDateFromLog(line));
                        var log = dateAndLog[1];
                        var row = table.insertRow(0);
                        var cell0 = row.insertCell(0);
                        cell0.innerHTML = date;
                        var cell1 = row.insertCell(1);
                        cell1.innerHTML = log;
                    }
                }

                var header = table.createTHead();
                var row = header.insertRow(0);
                var cell0 = row.insertCell(0);
                cell0.innerHTML = "Time";
                var cell1 = row.insertCell(1);
                cell1.innerHTML = "Log";
            }
        }
    }
    rawFile.send(null);    
};



