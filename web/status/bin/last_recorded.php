<?php

function time_elapsed($date) {
    $periods         = array("sec", "min", "hour", "day", "week", "month", "year", "decade");
    $lengths         = array("60","60","24","7","4.35","12","10");
    
    $now             = time();
    $unix_date       = $date;
    
    // check validity of date
    if(empty($unix_date)) {    
        return "Bad date";
    }

    $difference    = $now - $unix_date;
    
    for($j = 0; $difference >= $lengths[$j] && $j < count($lengths)-1; $j++) {
        $difference /= $lengths[$j];
    }
    
    $difference = round($difference);
    
    if($difference != 1) {
        $periods[$j].= "s";
    }
    
    return "$difference $periods[$j]";
}

// Get the microscope parameter m from URL
$m = htmlspecialchars($_REQUEST["m"]);

$filename = '../logs/' . $m . '.last';

if (file_exists($filename)) {
    //Get the last recorded time from file
    $last_time = file($filename);
    
    //Convert to php time
    $old_time = $last_time[0];
    $old_time = $old_time/1000;

    //get the difference
    $diff = time_elapsed($old_time);
    echo $diff;
    
} else {
    echo "NA";
}

?>