function refershStatus(microscope) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var file = path.substr(0, idx) + "/../logs/" + microscope + ".last";

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
                var currentMSec = Date.parse(Date().toString());
                var lastLogMSec = Number(rawFile.responseText);
                var toleranceMSec = 6*60*60*1000; //15 Minutes
                var dateStr = getDateString(new Date(lastLogMSec));
                var status = {message:"Idle", icon:" fa-exclamation-circle"};
                status.message = dateStr;
                if(currentMSec - lastLogMSec < toleranceMSec) {
                    status.icon = "fa-check-circle";
                };
                document.getElementById(microscope + "-status-key").innerHTML = "Last recorded at:";
                document.getElementById(microscope + "-status-value").innerHTML = status.message;
                document.getElementById(microscope + "-status-icon").innerHTML = '<i class="fa ' + status.icon + ' fa-5x"></i>';
            }
        }
    }
    rawFile.send(null);
};



