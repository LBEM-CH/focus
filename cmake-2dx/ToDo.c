/******************************************************************************************************/
/*                                                                                                    */
/*                                ToDo.c Bug/Feature Tracking File                                    */
/*                                                                                                    */
/* Format:                                                                                            */
/*  Sections: ###### Section Name ###### | Used to indicate program.                                  */
/*  Items: (!{Response}) -(-...) ([{Priority}]) Bug/Feature                                           */
/*    e.x. - [1] Fix main.cpp                                                                         */
/*         -- [3] Fix Resolution issue                                                                */
/*         ! -- 1024x768 scaling is intentional, is this the problem?                                 */
/*         -- [1] Fix compile issues                                                                  */
/*         --- libstdc++.so.6 missing                                                                 */
/*                                                                                                    */
/*  Completion: Standard C++ commenting indicates Bug/Feature fixed/implemented.                      */
/*                                                                                                    */
/*  Notes:                                                                                            */
/*    Priority indicator for items optional (blank indicates default priority)                        */
/*    Low numbers indicate high priority, i.e. 1 higher importance than 5, etc.                       */
/*                                                                                                    */
/*    *This file exists as a useful tool so long as our development team is small                     */
/*     eventually bug-tracking software will likely be needed. (BugZilla, etc)                        */
/*                                                                                                    */
/*******************************************************************************************************/

- [1 Henning] ctf_search 


#########################################################################################################
######################        All Programs        #######################################################
#########################################################################################################
//Freezing release of 2dx_image-2.0.5 revision: 810  (Feb. 7, 2007)
//Freezing release of 2dx_image-2.1.0 revision: 1394 (April 25, 2007)
//Freezing release of 2dx-3.0.0       revision: 882  (November 19, 2007)  includes 2dx_merge and ML code.


- [3] A note in the log-file browser to inform the user that a large logfile (>10kB) has been truncated.

- Add user based configuration/state files in ~/.2dx
-- 2dx_image.state
-- 2dx_merge.state
-- scripts-standard/custom
-- etc

- [4] Should the software update feature only offer "Ignore this version" "Shown me the installer" "Not now"? This way, we would not loose future updates for these users ?
-- [4] Should we have the option "Don't check for updates" in a preferences pane? Or should the program always try to check for updates?

- [9] Just a thought as I was processing images:
-- Dynamic workflows, customizable by user. 
-- Generate workflow from unified script "pool" which can be organized as desired.
-- Include apple-script like organization of the form
--- Start
--- Do 10 times
--- Run this script
--- Change this variable to this value
--- Run that script
--- End Do
--- Open this image for display
--- End

#########################################################################################################
######################          MRC and own code         ################################################
#########################################################################################################

- [2] 2dx_byteswap.exe fails on the spider image, with this error message:
Byte swap has produced an invalid header: 0 0 0 Should be 1 2 3 
Byte swap of merge-p22121-scaled.mrc failed. 

#########################################################################################################
######################          2dx_merge         #######################################################
#########################################################################################################

- [2] The Album should only display selected images, when that option is choosen.
- [2] The Album should have the same image selected as in the GUI of 2dx_merge.
- [2] The Album should indicate if an image is selected in the 2dx_merge GUI, or not. This could be done with a grey overlay for the non-selected images? Or with a little click box next to the image? Or both?

- [3] When the user quits 2dx_merge, but the data table has non-saved changes, there should be that dialogue "Save&Quit?, ..."

- [3] Change color scheme for Merger.
-- Try to find complimentary color for Black gradient for viewContainer headers.

- [3] We mention in the manuscript a full-screen browser for the 2dx_merge program, which works in phase-origin units rather than pixels... We still could save that for 2dx_3.0.1 or so.
-- The full-screen browser for 2dx_merge will never apply to large raw images, and never to FFTs. It will only apply to reconstructed maps, which are small, or volumes.
-- This full-screen browser therefore should only have the following options:
--- Show Full Screen
--- Display Coordinate Info (not in pixles but phases (200px=360deg)
--- Show Mouse Button Assignment
--- Screen Shot
--- Set Phase Origin (I then can implement a script for the one selected image)
--- View Tilt Axis (using TAXA / TANGL)
--- Zoom
--- Help
--- Close.

Not there are:
--- View Lattice
--- Selection based FFT
--- Polygonal Selection
--- Unbending References

- [4] The text in the "Standard Scripts" pane is within a column that is limited on the right end by the iteration counter "5" of the Merge&Refine script. On Xiangyan's Linux the font is so small that the script titles are not readable.

- [3] The line height of the image lines in the Import window are not the same on the left and the right panel.

- [4] Show only selected directories affects album  model
-- Selection works between album viewer and project model

- [5] Add not-full-screen browser that needs to work only on 2D result images (for the moment).
-- Display of header info
-- Display of other selected parameters (from a config file), like defocus, PhaseOri, imagenumber that belong to that image
-- when clicking on one *.mrc image, open navigator similar to 2dx_image navigator, except that the image dimensions are in phaseorigin space. 0 is left border of unit cell, 360 is one unit cell further, and the tilt axis is defined by TAXA/TANGL, not TLTAXA/TLTANG.
-- Possibility to click into the correctly scaled and sheared result map, and use the click-coordinates as new phase origin values. The Center of the map would thereby correspond to the current phase origin. A click half a unit-cell to the right (A direction) would correspond to an ADDITIONAL 180 deg phase origin shift in X.  A click half a unit cell in the direction of the other axis (B direction, can be different than vertical) would correspond to 180 deg additional phaseshift in Y. This tool would allow very quickly to re-center an image to the correct phase origin. 
-- If the full-screen browser of 2dx_image.app continues dealing with real-space and fourier-space images in pixel coordinates, then the full-screen browser in 2dx_merge.app could deal with images in unit-cell space. That means, a unit cell is always 200 pixels wide, the current setting in 2dx_genergeMergeMap.script . The vertical dimension of the unit cell depends on the shearing and scaling according to the header, though.

- [8] For 3D MAP volumes, as preview have a slider that allows screening up and down through the stack of 2D images in that volume

- [4 Henning] scalimamp... for calculation of internal B-factor => image quality.    ;    Or use BR or formfactor as reference.
- [4 Henning] The output of mmbox.exe is strongly dependent on the phase origin being in the center of the good crystal area ... (?)


#########################################################################################################
######################          2dx_image         #######################################################
#########################################################################################################

- [3] In the fullscreen browser for FFTs -> Lattice Determination: Instead of "Accept as Second Lattice", there could be the following buttons: "Swap first and second lattice" , "Copy first onto second lattice" , "Copy second onto first lattice". This way, the current spot selection scheme and "Accept as first lattice" could remain. The user would always only work on the "first lattice", but could use the second lattice position as safe heaven for a determined lattice, and then try manually to find other lattices. If these turn out to not work, the user could copy back the previous attempt onto the "first lattice" register.
-- That pop-up window should then display the lattice vectors for both lattices, so that the user can see which lattice is in which register.

- [5] The panel maximize buttons also for 2dx_image, as is in 2dx_merge?

- [4] It would be nice to have a display of the currently available harddrive space in the bottom footer section of the GUI. Same for 2dx_merge.

- [9] Console output for 2dx_merge/2dx_image

- [2] Use Centric/Allspace information to complete the Friedel symmetric partners for all relevent symmetry groups (P2, P4, etc)

- [5] This would be toooooo cool: A button top right in the header bar of the GUI of 2dx_image, that would shrink the GUI into a small window of 300x100 pixels dimension that only displays the progress bar with its title.

- [5 Henning] Automatic masking should also offer the possibility to crop into smaller, square image.
- [3 Henning] check synthetic unbending phase origin with respect to revxsgn, etc.

- [8] Continue to look into the 2dx_tt*.for TDXCONV issues. Most should be worked out by now, 2dx_ttboxk.for still seg-faults during execution.

- [9] Maybe an adaptive SNR based peaksearch or just isolate 2 or 3 sigma outliers based on CTF. Either way a more flexible peak criterion is needed.

- [9] Look into accuracy issues in fft relative to peaksearch. Perhaps try double fftw intermediary (converting back to float at the end)

- [9] A search parameters bar?

#########################################################################################################
######################          2dx_logbrowser    #######################################################
#########################################################################################################
 
- [3] or all into <HTML> with JAVA header, and Firefox.


#########################################################################################################
######################          2dx.org           #######################################################
#########################################################################################################

- Manual:
-- Tutorial "how to modify the scripts" (Bryant)
-- Tutorial "how to determine optimal processing parameters" (Who?)
-- Tutorial "how to determine the resolution"  (Henning)
-- Tutorial ECW2010?

