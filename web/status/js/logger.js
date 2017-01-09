function createLogTable(microscope) {
    var path = window.location.href.toString();
    var idx = path.lastIndexOf('/');
    var filename = path.substr(0, idx) + "/../pages/logtable.php?m=" + microscope;

    var xmlhttp;
    if (window.XMLHttpRequest) {
        xmlhttp = new XMLHttpRequest();
    } else {
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }

    xmlhttp.onreadystatechange = function () {
        if (this.readyState == 4 && this.status == 200) {
            document.getElementById("log-table").innerHTML = this.responseText;
            $('#log-table').DataTable({
                paging: true,
                stateSave: true,
                "order": [[0, 'desc']]
            });
        }
    };

    xmlhttp.open("GET", filename, true);
    xmlhttp.send();
}



