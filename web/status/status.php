<?php

//Get the configurations
$configs = include('config.php');

//Setup microscope
$m = $configs->microscopes[0];
if(isset($_REQUEST["m"])) {
    $m = htmlspecialchars($_REQUEST["m"]);
};

// Create a title with first letter capital
$title = ucfirst($m);

$correct_microscope = in_array($m, $configs->microscopes);

echo '
<!DOCTYPE html>
<html lang="en">
    
    <head>
        <meta http-equiv="refresh" content="' . $configs->refresh_interval . '" >
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">

        <title>'. $configs->title . ' | ' . $title .'</title>

        <!-- Bootstrap Core CSS -->
        <link href="vendor/bootstrap/css/bootstrap.min.css" rel="stylesheet">

        <!-- Custom CSS -->
        <link href="dist/css/sb-admin-2.css" rel="stylesheet">

        <!-- DataTables CSS -->
        <link href="vendor/datatables-plugins/dataTables.bootstrap.css" rel="stylesheet">

        <!-- DataTables Responsive CSS -->
        <link href="vendor/datatables-responsive/dataTables.responsive.css" rel="stylesheet">

        <!-- Custom Fonts -->
        <link href="vendor/font-awesome/css/font-awesome.min.css" rel="stylesheet" type="text/css">

        <!-- Google Chrome Add to homescreen settings -->
        <link rel="manifest" href="/manifest.json">

        <!-- Safari Add to homescreen settings -->
        <link rel="apple-touch-icon" href="../logos/icon-256x256.png">
        <link rel="apple-touch-startup-image" href="../logos/logo-small.png">
        <meta name="apple-mobile-web-app-title" content="Focus Status">
        <meta name="apple-mobile-web-app-capable" content="yes">
        <meta name="apple-mobile-web-app-status-bar-style" content="blue">

    </head>
';

if($correct_microscope) {
    echo '
    <body onload="refreshData();">
    ';
} else {
    echo '
    <body>
    ';
}

echo '
        <div id="wrapper">
            <nav class="navbar navbar-default navbar-static-top" role="navigation" style="margin-bottom: 0">
                <div class="navbar-header">
                    <a class="navbar-brand" href="' . $configs->company_website .'">
                        <img class="pull-left" src="logos/company-logo.png" style="height: 16px;"></img>&nbsp; | Status
                    </a>
                </div>
                <!-- /.navbar-header -->
                <ul class="nav navbar-top-links navbar-right">
                ';

foreach($configs->microscopes as $mItem) {
    echo '
                    <li><a href="status.php?m=' . $mItem . '">' . ucfirst($mItem) . '</a></li>
    ';
};
echo '
                </ul>
                <!-- /.navbar-top-links -->

            </nav>
            
            <div id="page-wrapper">
                <div class="row">
                    <div class="col-lg-12">
                        <h2 class="page-header"> ' . $title . '
                        <small>
                            (Last recorded: <span id="status-value">NA</span> ago)
                        </small>
                        </h2>
                    </div>
                    <!-- /.col-lg-12 -->
                </div>
';

if($correct_microscope) {
echo '
                <div class="row">
                    <div class="col-lg-12">
                        <h3 class="page-header">Activity
                        </h3>
                    </div>
                    <!-- /.col-lg-12 -->

                    <div class="panel-heading">
                        <span class="lead" id="recent-range"></span>
                        <div class="pull-right">
                            <span class="btn-group">
                                <button type="button" class="btn btn-success btn-sm dropdown-toggle" data-toggle="dropdown">
                                    <i class="fa fa-clock-o"></i> Change Range <span class="caret"></span>
                                </button>
                                <ul class="dropdown-menu" role="menu">
                                    <li onclick=\'refreshRecent(60*60);\'><a href="#">Last 1 hour</a></li>
                                    <li onclick=\'refreshRecent(3*60*60);\'><a href="#">Last 3 hours</a></li>
                                    <li onclick=\'refreshRecent(6*60*60);\'><a href="#">Last 6 hours</a></li>
                                </ul>
                            </span>
                        </div>
                    </div>
                    
                </div>
                <div class="row">
                    <div class="col-lg-3 col-md-6">
                        <div class="panel panel-primary">
                            <div class="panel-heading">
                                <div class="row">
                                    <div class="col-xs-3">
                                        <i class="fa fa-dot-circle-o fa-5x"></i>
                                    </div>
                                    <div class="col-xs-9 text-right">
                                        <div class="huge" id="last-recorded"></div>
                                        <div>images recorded</div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-lg-3 col-md-6">
                        <div class="panel panel-green">
                            <div class="panel-heading">
                                <div class="row">
                                    <div class="col-xs-3">
                                        <i class="fa fa-arrow-circle-o-down fa-5x"></i>
                                    </div>
                                    <div class="col-xs-9 text-right">
                                        <div class="huge" id="last-imported">0</div>
                                        <div>images imported</div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-lg-3 col-md-6">
                        <div class="panel panel-yellow">
                            <div class="panel-heading">
                                <div class="row">
                                    <div class="col-xs-3">
                                        <i class="fa fa-play-circle fa-5x"></i>
                                    </div>
                                    <div class="col-xs-9 text-right">
                                        <div class="huge" id="last-processed">0</div>
                                        <div>images processed</div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-lg-3 col-md-6">
                        <div class="panel panel-red">
                            <div class="panel-heading">
                                <div class="row">
                                    <div class="col-xs-3">
                                        <i class="fa fa-exclamation-circle fa-5x"></i>
                                    </div>
                                    <div class="col-xs-9 text-right">
                                        <div class="huge" id="last-errors">0</div>
                                        <div>processing errors</div>
                                    </div>
                                </div>
                            </div> 
                        </div>
                    </div>
                </div>
                <!-- /.row -->
                
                <div class="row">
                    <div class="col-lg-12">
                        <h3 class="page-header">Statistics</h3>
                        <h5>Showing data in the time range:</h5>
                    </div>
                    <div class="panel-heading">
                        <span class="lead" id="plot-start-date"></span> - <span class="lead" id="plot-end-date"></span>
                        <div class="pull-right">
                            <span class="btn-group">
                                <button type="button" class="btn btn-success btn-sm dropdown-toggle" data-toggle="dropdown">
                                    <i class="fa fa-clock-o"></i> Change Range <span class="caret"></span>
                                </button>
                                <ul class="dropdown-menu" role="menu">
                                    <li onclick=\'refreshPlots(3);\'><a href="#">Last 3 hrs</a></li>
                                    <li onclick=\'refreshPlots(6);\'><a href="#">Last 6 hrs</a></li>
                                    <li onclick=\'refreshPlots(12);\'><a href="#">Last 12 hrs</a></li>
                                    <li onclick=\'refreshPlots(24);\'><a href="#">Last 24 hrs</a></li>
                                    <li class="divider"></li>
                                    <li onclick=\'refreshPlots(2*24);\'><a href="#">Last 2 days</a></li>
                                    <li onclick=\'refreshPlots(3*24);\'><a href="#">Last 3 days</a></li>
                                    <li onclick=\'refreshPlots(7*24);\'><a href="#">Last 7 days</a></li>
                                    <li onclick=\'refreshPlots(14*24);\'><a href="#">Last 14 days</a></li>
                                </ul>
                            </span>
                        </div>
                    </div>
                    <!-- /.col-lg-12 -->
                </div>
                <!-- /.row -->

                <div class="row">
                    <div class="col-md-12">
                        <div class="panel panel-info">
                            <div class="panel-heading">
                                Various statistics (Select the area in this chart to restrict the time range in all charts)
                            </div>
                            <div class="panel-body">
                                <div class="flot-chart">
                                    <div class="flot-chart-content" id="log-all-plot"></div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="panel panel-info">
                            <div class="panel-heading">
                                Mean Pixel Values of Raw Stacks [Counts/px/frame]
                            </div>
                            <div class="panel-body">
                                <div class="flot-chart">
                                    <div class="flot-chart-content" id="log-mean-plot"></div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <!-- /.col-lg-4 -->
                    <div class="col-md-6">
                        <div class="panel panel-info">
                            <div class="panel-heading">
                                Total Sample Drift [Angstroms]
                            </div>
                            <div class="panel-body">
                                <div class="flot-chart">
                                    <div class="flot-chart-content" id="log-drift-plot"></div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="panel panel-info">
                            <div class="panel-heading">
                                Iciness - The Relative Amount of Crystalline Ice in the Image [a.u.], (should stay below 1.0)
                            </div>
                            <div class="panel-body">
                                <div class="flot-chart">
                                    <div class="flot-chart-content" id="log-iciness-plot"></div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="panel panel-info">
                            <div class="panel-heading">
                                Defocus Fit [Micrometers]
                            </div>
                            <div class="panel-body">
                                <div class="flot-chart">
                                    <div class="flot-chart-content" id="log-defocus-plot"></div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="panel panel-info">
                            <div class="panel-heading">
                                Resolution of CTF Fit [Angstroms], (lower is better)
                            </div>
                            <div class="panel-body">
                                <div class="flot-chart">
                                    <div class="flot-chart-content" id="log-resolution-plot"></div>
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="panel panel-info">
                            <div class="panel-heading">
                                CC Value of CTF Fit [a.u.], (higher is better)
                            </div>
                            <div class="panel-body">
                                <div class="flot-chart">
                                    <div class="flot-chart-content" id="log-ccvalue-plot"></div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                <!-- /.row -->

                <div class="row">
                    <div class="col-lg-12">
                        <h3 class="page-header">Recent Thumbnails</h3>
                    </div>
                    <div class="col-lg-12">
                        <div class="col-md-4">
                            <img class="img-thumbnail" src="http://placehold.it/400x400" height="400" width="400" alt=""  id="thumbnail-0">
                        </div>
                        <div class="col-md-4">
                            <img class="img-thumbnail" src="http://placehold.it/400x400" height="400" width="400" alt=""  id="thumbnail-1">
                        </div>
                        <div class="col-md-4">
                            <img class="img-thumbnail" src="http://placehold.it/400x400" height="400" width="400" alt="" id="thumbnail-4">
                        </div>
                    </div>
                    <div class="col-lg-12">
                        <div class="col-md-4">
                            <img class="img-thumbnail" src="http://placehold.it/400x400" height="400" width="400" alt="" id="thumbnail-2">
                        </div>
                        <div class="col-md-4">
                            <img class="img-thumbnail" src="http://placehold.it/400x400" height="400" width="400" alt="" id="thumbnail-3">
                        </div>
                        <div class="col-md-4">
                            <img class="img-thumbnail" src="http://placehold.it/400x400" height="400" width="400" alt="" id="thumbnail-5">
                        </div>
                    </div>
                </div>
                <!-- /.row -->
                <div class="row">
                    <div class="col-lg-12">
                        <h3 class="page-header">Logs</h3>
                    </div>
                    <div class="col-lg-12">
                        <table width="100%" class="table table-striped table-bordered table-hover" id="log-table">
                        </table>
                    </div>
                    <!-- /.col-lg-12 -->
                </div>
';
}
else {
echo '
                <div class="row">
                    <div class="col-lg-12">
                        <p>This microscope is not configured.</p>
                    </div>
                    <!-- /.col-lg-12 -->
                </div>
';
};

echo '      </div>
            <!-- /#page-wrapper -->

            <div class="panel panel-default text-center" role="footer" style="margin-bottom: 0">
                <div class="panel-heading">
                    <p class="text-center">Powered by:</p>
                    <p class="text-center"> <a href="http://www.focus-em.org"><img src="logos/logo-small.png" style="height: 60%;"></img></a></p>
                    <p class="text-center" >Please cite <a href="https://doi.org/10.1101/105452">Biyani et. al. (2017), bioRXiv</a>, if you use this service.</p>
                </div>
            </div>
        </div>
        
        <!-- /#wrapper -->


        <!-- jQuery -->
        <script src="vendor/jquery/jquery.min.js"></script>

        <!-- Bootstrap Core JavaScript -->
        <script src="vendor/bootstrap/js/bootstrap.min.js"></script>

        <!-- Custom Theme JavaScript -->
        <script src="dist/js/sb-admin-2.js"></script>

        <!-- DataTables JavaScript -->
        <script src="vendor/datatables/js/jquery.dataTables.min.js"></script>
        <script src="vendor/datatables-plugins/dataTables.bootstrap.min.js"></script>
        <script src="vendor/datatables-responsive/dataTables.responsive.js"></script>

        <!-- Mean and resolution plots -->
        <script src="vendor/flot/excanvas.min.js"></script>
        <script src="vendor/flot/jquery.flot.js"></script>
        <script src="vendor/flot/jquery.flot.pie.js"></script>
        <script src="vendor/flot/jquery.flot.resize.js"></script>
        <script src="vendor/flot/jquery.flot.time.js"></script>
        <script src="vendor/flot/jquery.flot.selection.js"></script>
        <script src="vendor/flot-tooltip/jquery.flot.tooltip.min.js"></script>
        <script src="js/plots.js"></script>

        <!-- Thumbnail updating javascript -->
        <script src="js/thumbnails.js"></script>

        <!-- Status updating javascript -->
        <script src="js/utilities.js"></script>
        <script src="js/logger.js"></script>    
        <script src="js/status.js"></script> 

        <script>
            function refreshPlots(lastHourCount) {
    
                setCookie("plotsLastHoursRange", lastHourCount);
                if(lastHourCount > 0) {
                    var startRange = new Date();
                    startRange.setTime(startRange.getTime() - lastHourCount * 60 * 60 * 1000);
                    plotLogData(Date.parse(startRange.toString()), -1, "' . $m . '");
                } else {
                    plotLogData(-1, -1, "' . $m . '");
                }
            }

            function refreshRecent(lastSeconds) {
                setCookie("recentsLastSecondsRange", lastSeconds);
                var lastHours = Math.round(lastSeconds/(60*60));
                var rangeStr = "In last " + lastHours + " hour";
                if(lastHours > 1) rangeStr += "s";
                document.getElementById("recent-range").innerHTML = rangeStr + " ...";
                refershStatus("' . $m . '", lastSeconds);
            }
            
            function refreshData() {
                
                loadThumbnails("' . $m . '");
                
                var lastRangeCookie = getCookie("plotsLastHoursRange");
                if(lastRangeCookie == ""){
                    refreshPlots(3);
                } else {
                    refreshPlots(Number(lastRangeCookie));
                }

                var lastSecsCookie = getCookie("recentsLastSecondsRange");
                if(lastSecsCookie == ""){
                    refreshRecent(60*60);
                } else {
                    refreshRecent(Number(lastSecsCookie));
                }
            }
        
            $(document).ready(function () {
                createLogTable("' . $m . '");
            });
        </script>

    </body>

</html>';


?>
