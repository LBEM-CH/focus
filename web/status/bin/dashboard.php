<!DOCTYPE html>
<html lang="en">

    <head>
        <meta http-equiv="refresh" content="180" >
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <meta name="C-CINA | Hardware Status" content="">
        <meta name="Nikhil Biyani" content="">

        <title>Status | Dashboard</title>

        <?php include 'safari-homescreen.php'; ?>

        <!-- Bootstrap Core CSS -->
        <link href="../vendor/bootstrap/css/bootstrap.min.css" rel="stylesheet">

        <!-- Custom CSS -->
        <link href="../dist/css/sb-admin-2.css" rel="stylesheet">

        <!-- DataTables CSS -->
        <link href="../vendor/datatables-plugins/dataTables.bootstrap.css" rel="stylesheet">

        <!-- DataTables Responsive CSS -->
        <link href="../vendor/datatables-responsive/dataTables.responsive.css" rel="stylesheet">


        <!-- Custom Fonts -->
        <link href="../vendor/font-awesome/css/font-awesome.min.css" rel="stylesheet" type="text/css">

        <link rel="manifest" href="/manifest.json">

        <!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->
        <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
        <!--[if lt IE 9]>
            <script src="https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js"></script>
            <script src="https://oss.maxcdn.com/libs/respond.js/1.4.2/respond.min.js"></script>
        <![endif]-->

    </head>

    <body onload="refreshStatus();">
        <?php include_once("analyticstracking.php") ?>

        <div id="wrapper">
            <?php
            include 'topbar.php';
            ?>
            <div id="page-wrapper">

                <div class="row">
                    <div class="col-lg-12">
                        <h3 class="page-header">Dashboard</h3>
                        <div class="list-group">
                            <a href="microscope.php?m=titan" class="list-group-item">
                                <i class="fa fa-clock-o fa-fw"></i> Titan
                                <span class="pull-right text-muted small"> 
                                    Last recorded: 
                                    <em><span id="titan-status-value"></span> ago</em>
                                </span>
                            </a>
                            <a href="microscope.php?m=polara" class="list-group-item">
                                <i class="fa fa-clock-o fa-fw"></i> Polara
                                <span class="pull-right text-muted small"> 
                                    Last recorded: 
                                    <em><span id="polara-status-value"></span> ago</em>
                                </span>
                            </a>
                        </div>
                    </div>
                </div>


                <div class="row">
                    <div class="col-lg-12">
                        <h4 class="page-header">Statistics</h4>
                        <h5 class="lead">
                            Data Range: 
                            <span class="text-info" id="plot-start-date"></span> 
                            - 
                            <span class="text-info" id="plot-end-date"></span>
                            <small>
                            <span class="btn-group">
                                <button type="button" class="btn btn-primary btn-sm dropdown-toggle" data-toggle="dropdown">
                                    <i class="fa fa-clock-o"></i> Change Range <span class="caret"></span>
                                </button>
                                <ul class="dropdown-menu" role="menu">
                                    <li onclick='refreshPlots(3);'><a href="#">Last 3 hrs</a></li>
                                    <li onclick='refreshPlots(6);'><a href="#">Last 6 hrs</a></li>
                                    <li onclick='refreshPlots(12);'><a href="#">Last 12 hrs</a></li>
                                    <li onclick='refreshPlots(24);'><a href="#">Last 24 hrs</a></li>
                                    <li class="divider"></li>
                                    <li onclick='refreshPlots(2*24);'><a href="#">Last 2 days</a></li>
                                    <li onclick='refreshPlots(3*24);'><a href="#">Last 3 days</a></li>
                                    <li onclick='refreshPlots(7*24);'><a href="#">Last 7 days</a></li>
                                    <li onclick='refreshPlots(14*24);'><a href="#">Last 14 days</a></li>
                                </ul>
                            </span>
                            </small>
                        </h5>
                        <div class="form-group">
                            <label class="text-muted">Select Microscopes: </label>
                            <label class="checkbox-inline">
                                <input type="checkbox" checked="checked" id="titan-check" onclick='saveMicroscopesSelection("titan");'>Titan
                            </label>
                            <label class="checkbox-inline">
                                <input type="checkbox" checked="checked" id="polara-check" onclick='saveMicroscopesSelection("polara");'>Polara
                            </label>
                        </div>
                    </div>
                    <!-- /.col-lg-12 -->
                </div>
                <!-- /.row -->
                <div class="row">
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
                                    <div class="flot-chart-content" id="log-icyness-plot"></div>
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

            </div>
            <!-- /#page-wrapper -->
            
            <?php include 'footer.php';?>
        </div>
        <!-- /#wrapper -->
        

        <!-- jQuery -->
        <script src="../vendor/jquery/jquery.min.js"></script>

        <!-- Bootstrap Core JavaScript -->
        <script src="../vendor/bootstrap/js/bootstrap.min.js"></script>

        <!-- Custom Theme JavaScript -->
        <script src="../dist/js/sb-admin-2.js"></script>

        <!-- DataTables JavaScript -->
        <script src="../vendor/datatables/js/jquery.dataTables.min.js"></script>
        <script src="../vendor/datatables-plugins/dataTables.bootstrap.min.js"></script>
        <script src="../vendor/datatables-responsive/dataTables.responsive.js"></script>

        <!-- Mean and resolution plots -->
        <script src="../vendor/flot/excanvas.min.js"></script>
        <script src="../vendor/flot/jquery.flot.js"></script>
        <script src="../vendor/flot/jquery.flot.pie.js"></script>
        <script src="../vendor/flot/jquery.flot.resize.js"></script>
        <script src="../vendor/flot/jquery.flot.time.js"></script>
        <script src="../vendor/flot/jquery.flot.selection.js"></script>
        <script src="../vendor/flot-tooltip/jquery.flot.tooltip.min.js"></script>
        <script src="../js/plots.js"></script>

        <!-- Thumbnail updating javascript -->
        <script src="../js/thumbnails.js"></script>

        <!-- Status updating javascript -->
        <script src="../js/utilities.js"></script>
        <script src="../js/logger.js"></script>    
        <script src="../js/status.js"></script>
        <script>
            function refreshPlots(lastHourCount) {
                var microscopes = [];
                if(document.getElementById("titan-check").checked == true) microscopes.push("titan");
                if(document.getElementById("polara-check").checked == true) microscopes.push("polara");
    
                setCookie("plotsLastHoursRange", lastHourCount);
                if(lastHourCount > 0) {
                    var startRange = new Date();
                    startRange.setTime(startRange.getTime() - lastHourCount * 60 * 60 * 1000);
                    plotLogData(Date.parse(startRange.toString()),-1, microscopes);
                } else {
                    plotLogData(-1, -1, microscopes);
                }
            }
            
            function saveMicroscopesSelection(microscope){
                var selected = document.getElementById(microscope+"-check").checked;
                var selectedStr = "true";
                if(selected == false) selectedStr = "false"; 
                setCookie("plots-"+microscope+"-selected", selectedStr);
                refreshStatus();
            }
            
            function refreshStatus() {
                refreshLastRecorded("titan");
                refreshLastRecorded("polara");
                
                var titanSelCookie = getCookie("plots-titan-selected");
                if(titanSelCookie == "false") {
                    document.getElementById("titan-check").checked = false;
                } else {
                    document.getElementById("titan-check").checked = true;
                }
                
                var polaraSelCookie = getCookie("plots-polara-selected");
                if(polaraSelCookie == "false") {
                    document.getElementById("polara-check").checked = false;
                } else {
                    document.getElementById("polara-check").checked = true;
                }
                
                var lastRangeCookie = getCookie("plotsLastHoursRange");
                if(lastRangeCookie == ""){
                    refreshPlots(3);
                } else {
                    refreshPlots(Number(lastRangeCookie));
                }
            }
        </script>


    </body>

</html>
