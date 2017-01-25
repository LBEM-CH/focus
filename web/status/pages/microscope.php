<!DOCTYPE html>
<html lang="en">
    
    <head>
        <meta http-equiv="refresh" content="60" >
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <meta name="C-CINA | Hardware Status" content="">
        <meta name="Nikhil Biyani" content="">

        <title>Status | <?php echo ucfirst(htmlspecialchars($_REQUEST["m"])); ?>
        </title>

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

        <div id="wrapper">
            <?php
            include 'topbar.php';
            ?>
            <div id="page-wrapper">
                <div class="row">
                    <div class="col-lg-12">
                        <h2 class="page-header"><?php echo ucfirst(htmlspecialchars($_REQUEST["m"])); ?>
                        </h2>
                    </div>
                    <!-- /.col-lg-12 -->
                </div>
                <div class="row">
                    <div class="col-lg-12">
                        <h4 class="page-header">Recent Status 
                   	    <small>
                           	(Last recorded: <span id="status-value">NA</span> ago)
                            </small>
                        </h4>
                    </div>
                    <!-- /.col-lg-12 -->
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
                                        <div>Images recorded in last hour</div>
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
                                        <div>Images imported in last hour</div>
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
                                        <div>Images processed in last hour</div>
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
                                        <div>Processing errors in last hour</div>
                                    </div>
                                </div>
                            </div> 
                        </div>
                    </div>
                </div>
                <div class="row">
                    <div class="col-lg-12">
                        <h4 class="page-header">Recent Thumbnails</h4>
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
                        <h4 class="page-header">Logs</h4>
                    </div>
                    <div class="col-lg-12">
                        <table width="100%" class="table table-striped table-bordered table-hover" id="log-table">
                        </table>
                    </div>
                    <!-- /.col-lg-12 -->
                </div>

            </div>
            <!-- /#page-wrapper -->

            <?php include 'footer.php'; ?>
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
        
        function getMicroscopeId() {
            return <?php echo '"' . htmlspecialchars($_REQUEST["m"]) . '"'; ?>
        }
            
        function refreshStatus() {
            refershStatus(getMicroscopeId());
            loadThumbnails(getMicroscopeId());
        }
        </script>

        <script>
            $(document).ready(function () {
                createLogTable(getMicroscopeId());
            });
        </script>

    </body>

</html>
