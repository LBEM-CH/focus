/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef ARGUMENT_TEMPLATE_HPP
#define	ARGUMENT_TEMPLATE_HPP

#include "tclap/CmdLine.h"
#include <string>

namespace volume
{
    namespace arguments
    {   
        typedef TCLAP::CmdLine Executable;
        typedef TCLAP::Arg Arg;
        
        namespace templates
        {
            
            static TCLAP::ValueArg<std::string> HKLIN("", "hklin", "input reflections file in hkl format(H K L AMP PHASE <FOM/DUMMY> <SIG_PHASE/DUMMY> <DUMMY>)", false, "", "FILE");
            static TCLAP::ValueArg<std::string> HKZIN("", "hkzin", "input reflections file in hkz format(H K z* AMP PHASE <FOM/DUMMY> <SIG_PHASE/DUMMY> <DUMMY>)", false, "", "FILE");
            static TCLAP::ValueArg<std::string> MRCIN("", "mrcin", "input mrc/map file", false, "", "FILE");
            static TCLAP::ValueArg<std::string> HKLOUT("", "hklout", "output reflections file in hkl format(H K L AMP PHASE FOM)", false, "", "FILE");
            static TCLAP::ValueArg<std::string> MRCOUT("", "mrcout", "output mrc/map file", false, "","FILE");
            static TCLAP::ValueArg<std::string> PDBOUT("", "pdbout", "output file in pdb format", false, "", "FILE");

            static TCLAP::ValueArg<double> NX("X", "nx", "number of points in X (required with hklin/hkzin)", false, 0,"FLOAT");
            static TCLAP::ValueArg<double> NY("Y", "ny", "number of points in Y (required with hklin/hkzin)", false, 0,"FLOAT");
            static TCLAP::ValueArg<double> NZ("Z", "nz", "number of points in Z (required with hklin/hkzin)", false, 0,"FLOAT");
            static TCLAP::ValueArg<double> GAMMA("g", "gamma", "real space cell angle gamma in degrees (range 90...180, default 90, recommended with hklin/hkzin) ", false, 90.0, "FLOAT");
            static TCLAP::ValueArg<std::string> SYMMETRY("s", "symmetry", "crystallographic symmetry (2D) present if any (default P1)", false, "P1", "STRING");
            static TCLAP::ValueArg<double> MAXRES("R", "res", "maximum expected resolution of the map (default 2.0)", false, 2.0, "FLOAT");
            static TCLAP::ValueArg<double> MAXAMP("a", "amp", "desired maximum amplitude value in the volume (a scaling will be done accordingly)", false, -1.0,"FLOAT");
            static TCLAP::ValueArg<double> THRESHOLD("t", "threshold", "desired density threshold (partially thresholded if used with refinement)", false, -1.0,"FLOAT");
            static TCLAP::ValueArg<int> SUBSAMPLE("", "subsample", "subsample to factor", false, 0,"INT");

            static TCLAP::ValueArg<int> BEADS("b", "beads", "number of beads to be used", false, 0,"INT");

            static TCLAP::ValueArg<std::string> REFIN("r", "refin", "reference mrc/map file", false, "","FILE");
            static TCLAP::ValueArg<int> ITERATIONS("i", "iterations", "Number of iterations", false, 0,"INT");
            static TCLAP::ValueArg<double> SLAB("", "slab", "The membrane height in ratio of the Z length of the volume", false, 1.0,"FLOAT");
            static TCLAP::ValueArg<std::string> TEMP_LOC("", "temp", "Folder to keep temporary files for each iteration (if not specified temp files will not be written)", false, "","FOLDER");

            static TCLAP::SwitchArg EXTENDED("", "extended", "Produce an output with 2X2X1 unit cells", false);
            static TCLAP::SwitchArg INVERTED("", "inverted", "Produce an output map with inverted hand", false);
            static TCLAP::SwitchArg PSF("", "psf", "Produce an Point Spread Function(PSF) output map (works only with mrcout)", false);
            static TCLAP::SwitchArg FULL_FOURIER("", "full-fourier", "Produce a full P1 Fourier space in output (noticeable only with hklout)", false);
            
        }
    }
}

#endif	/* ARGUMENT_TEMPLATE_HPP */

