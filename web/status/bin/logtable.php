<?php
// Get the microscope parameter m from URL
$m = htmlspecialchars($_REQUEST["m"]);

//CSV read options
$length = 150;        	//number of characters in a line
$delimiter = ';';
$limit = 1000;      	//Maximum number of lines read
$dataCount = 6;		//Number of data columns expected

$filename = '../logs/' . $m . '.data';

if (file_exists($filename)) {
    echo '<thead>
            <tr>
                <th>Recorded Time</th>
                <th>Defocus</th>
                <th>Resolution</th>
                <th>Mean</th>
                <th>Drift</th>
                <th>Iciness</th>
                <th>Ccvalue</th>
            </tr>
        </thead>';

    $count = 0;
    $f = fopen($filename, "r");

    while (($line = fgetcsv($f, $length, $delimiter)) !== false) {
        $count = $count + 1;
        
        //Break if reaches limit
        if($count > $limit) {
            break;
        }
        
        echo '<tr>';
        
        //Write the date
        $timestamp = $line[0]/1000;
        echo '<td>' . gmdate("Y-m-d H:i:s", $timestamp) . '</td>';
        
	    //Fill in missing values
        for($idx = count($line); $idx <= $dataCount; $idx++) {
	       $line[$idx] = 0.0;
	    }

        //Write the values
        for ($idx = 1; $idx <= $dataCount; $idx++) {
	    $cell = round(htmlspecialchars($line[$idx]), 2);
            echo '<td>' . $cell . '</td>';
        }

        echo '</tr>';
    }

    fclose($f);
} else {
    echo 'No logs found.';
}

?>
