[FOCUS](http://www.focus-em.org): High throughput processing of the image stacks recorded by electron microscopes
======

**About**  
A Graphical User Interface designed using C++ and Qt5 with following features:  
* An import tool which can continuously monitor the new images
* Scripts which can process these images using in-house and external software. It is easy for the users to define more scripts if required
* A parallel batch queue processor which transfers processing of the scripts to different resources available on the system
* A library which summaries the statistics of the collected images
* A website which can be used to continuously monitor the status of the recorded images

**Supported Modes**  
Focus supports different type of electron microscopy projects which can be set when setting up a new project. Current supported modes are:  
* Drift correction only
* Single Particle with drift correction
* 2D Electron Crystallography with drift correction
* Electron tomography with drift correction

**List of dependencies and installation instructions:** http://lbem-focus.epfl.ch/wiki/doku.php?id=1_0:install-source  
**Setting up external software tools:** http://lbem-focus.epfl.ch/wiki/doku.php?id=1_0:external-tools  

**General documentation:** https://lbem-focus.epfl.ch/wiki/doku.php?id=home  

If FOCUS is useful in your work, please cite:  

Biyani, N., Righetto, R. D., McLeod, R., Caujolle-Bert, D., Castano-Diez, D., Goldie, K. N., & Stahlberg, H. (2017). FOCUS: The interface between data collection and data processing in cryo-EM. Journal of Structural Biology, 198(2), 124–133. https://doi.org/10.1016/j.jsb.2017.03.007
