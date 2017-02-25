function loadThumbnails(microscope) {
    thumbnailNames = ["1-image.jpg", "1-FFT.jpg", "2-image.jpg", "2-FFT.jpg", "3-image.png", "4-image.jpg"];
    for(t=0; t<thumbnailNames.length; t++) {
        var path = window.location.href.toString();
        var idx = path.lastIndexOf('/');
        var file = path.substr(0, idx) + "/thumbnails/" + microscope + "-" + thumbnailNames[t];
        
        if(fileExists(file)) {
            document.getElementById("thumbnail-"+t).src = file;
        } else {
            document.getElementById("thumbnail-"+t).src = "https://placehold.it/400x400";
        }
    }
}
