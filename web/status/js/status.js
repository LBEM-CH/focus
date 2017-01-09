function refershStatus(microscope) {

    refreshLastRecorded(microscope);
    refreshLastData(microscope, "recorded", 60*60);
    refreshLastData(microscope, "imported", 60*60);
    refreshLastData(microscope, "processed", 60*60);
    refreshLastData(microscope, "errors", 60*60);
};

function refreshLastRecorded(microscope) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var filename = path.substr(0, idx) + "/../pages/last_recorded.php?m=" + microscope;

    var xmlhttp;
    if (window.XMLHttpRequest) {
        xmlhttp = new XMLHttpRequest();
    } else {
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }

    xmlhttp.onreadystatechange = function () {
        if (this.readyState == 4 && this.status == 200) {
            var element1 =  document.getElementById(microscope + "-status-value");
            if (typeof(element1) != 'undefined' && element1 != null) {
                document.getElementById(microscope + "-status-value").innerHTML = this.responseText;
            }
            
            var element2 =  document.getElementById("status-value");
            if (typeof(element2) != 'undefined' && element2 != null) {
                document.getElementById("status-value").innerHTML = this.responseText;
            }
            
        }
    };

    xmlhttp.open("GET", filename, true);
    xmlhttp.send();
};

function refreshLastData(microscope, type, interval) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var filename = path.substr(0, idx) + "/../pages/last_count.php?m=" + microscope + "&t=" + type + "&i=" + interval;

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
