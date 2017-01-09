function isSameDate(date1, date2) {
    if(date1.getDate() == date2.getDate() && 
        date1.getMonth() == date2.getMonth() && 
        date1.getFullYear() == date2.getFullYear()) {
        return true;
    } else return false;
};

function getDateFromLog(logLine) {
    var dateAndLog = logLine.split("@@");
    var dateStr = dateAndLog[0].replace(" ", "");
    logDate = new Date(Number(dateStr));
    return logDate;
};

function getDateString(logDate) {
    var currDate = new Date();
    var dateStr;
    
    if(isSameDate(logDate, currDate)) dateStr = "Today, ";
    else dateStr = logDate.toLocaleDateString() + ", ";
    dateStr += logDate.toLocaleTimeString();
    
    return dateStr;
};

function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

function fileExists(url) {
    if(url){
        var req;
        if (window.XMLHttpRequest) req= new XMLHttpRequest();               
        else req = new ActiveXObject("Microsoft.XMLHTTP");
        req.open('GET', url, false);
        req.send();
        return req.status==200;
    } else {
        return false;
    }
};

function setCookie(cname, cvalue) {
    document.cookie = cname + "=" + cvalue + ";path=/";
}

function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i = 0; i <ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') {
            c = c.substring(1);
        }
        if (c.indexOf(name) == 0) {
            return c.substring(name.length,c.length);
        }
    }
    return "";
}

