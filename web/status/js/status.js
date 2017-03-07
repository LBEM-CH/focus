function refershStatus(microscope, lastSeconds) {

    refreshLastRecorded(microscope);
    refreshLastData(microscope, "recorded", lastSeconds);
    refreshLastData(microscope, "imported", lastSeconds);
    refreshLastData(microscope, "processed", lastSeconds);
    refreshLastData(microscope, "errors", lastSeconds);
};

function refreshLastRecorded(microscope) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var filename = path.substr(0, idx) + "/bin/last_recorded.php?m=" + microscope;

    var xmlhttp;
    if (window.XMLHttpRequest) {
        xmlhttp = new XMLHttpRequest();
    } else {
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }

    xmlhttp.onreadystatechange = function () {
        if (this.readyState == 4 && this.status == 200) {
            document.getElementById("status-value").innerHTML = this.responseText;
        }
    };

    xmlhttp.open("GET", filename, true);
    xmlhttp.send();
};

function refreshLastData(microscope, type, interval) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var filename = path.substr(0, idx) + "/bin/last_count.php?m=" + microscope + "&t=" + type + "&i=" + interval;

    var xmlhttp;
    if (window.XMLHttpRequest) {
        xmlhttp = new XMLHttpRequest();
    } else {
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }

    xmlhttp.onreadystatechange = function () {
        if (this.readyState == 4 && this.status == 200) {
            document.getElementById("last-" + type).innerHTML = this.responseText;
        }
    };

    xmlhttp.open("GET", filename, true);
    xmlhttp.send();
};
