<?php

// Get the microscope and type parameter from URL
$m = htmlspecialchars($_REQUEST["m"]);
$t = htmlspecialchars($_REQUEST["t"]);
$i = htmlspecialchars($_REQUEST["i"]);

$filename = '../logs/' . $m . '.last_' . $t;

if (file_exists($filename)) {
    //Get the last times from file
    $timesArray = file($filename);
    
    $count = 0;
    $tolerance = $i;
    $nowtime = time();
    foreach ($timesArray as $lastTime) {
        if($nowtime - $lastTime/1000 <= $tolerance) {
            $count = $count + 1;
        } else {
	    break;
	}
    }
    echo $count;
    
} else {
    echo "0";
}

?>
