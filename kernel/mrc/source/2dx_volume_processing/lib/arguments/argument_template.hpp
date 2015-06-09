/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef ARGUMENT_TEMPLATE_HPP
#define	ARGUMENT_TEMPLATE_HPP

#include <string>

namespace volume
{
    namespace arguments
    {   
        /**
         * A class to store all possible Command Line Argument templates
         */
        class Argument
        {
            
        private:
       
            std::string _name;
            std::string _identifier;
            std::string _help;
            bool _has_arg;
         
            
        public:
            
            Argument(std::string name, std::string identifier, std::string help, bool has_arg=true)
            {
                this->_name = name;
                this->_identifier = identifier;
                this->_help = help;
                this->_has_arg = has_arg;
            };
            
            std::string name() { return _name; };
            std::string identifier() { return _identifier; };
            std::string help() { return _help; };
            bool has_arg() { return _has_arg; };
              
            //Generic arguments    
            static const Argument hklin;
            static const Argument hkzin;
            static const Argument mrcin;
            static const Argument hklout;
            static const Argument mrcout;
            static const Argument refin;
            static const Argument nx;
            static const Argument ny;
            static const Argument nz;
            static const Argument gamma;
            static const Argument symmetry;
            static const Argument max_resolution;
            static const Argument max_amplitude;
            static const Argument density_threshold;
            
            //Bead model generation args
            static const Argument number_of_beads;
            
            //Refinement specific arguments
            static const Argument number_of_iterations;
            static const Argument membrane_slab;
            static const Argument temp_loc;

            //Properties of output hkl/map file
            static const Argument extended;
            static const Argument inverted;
            static const Argument psf;
            static const Argument full_fourier;
            
            //Properties of input hkl/map file
            static const Argument info;
        };
        
        //Initialization of the arguments
        const Argument Argument::hklin("hklin", "l", "input reflections file in H K L AMP PHASE FOM/DUMMY <SIG_PHASE>/DUMMY <DUMMY> format");
        const Argument Argument::hkzin("hkzin", "z", "input reflections file in H K Z* AMP PHASE FOM/DUMMY <SIG_PHASE>/DUMMY <DUMMY> format");
        const Argument Argument::mrcin("mrcin", "m", "input mrc/map file");
        const Argument Argument::hklout("hklout", "L", "output reflections file in H K L AMP PHASE FOM column format");
        const Argument Argument::mrcout("mrcout", "M", "output mrc/map file");

        const Argument Argument::nx("nx", "X", "number of points in X (required with hklin/hkzin)");
        const Argument Argument::ny("ny", "Y", "number of points in Y (required with hklin/hkzin)");
        const Argument Argument::nz("nz", "Z", "number of points in Z (required with hklin/hkzin)");
        const Argument Argument::gamma("gamma", "g", "cell angle gamma in degrees (default 90, recommended with hklin/hkzin) ");
        const Argument Argument::symmetry("symm", "s", "crystallographic symmetry (2D) present if any (default P1)");
        const Argument Argument::max_resolution("res", "R", "maximum expected resolution of the map (default 2.0)");
        const Argument Argument::max_amplitude("amp", "a", "desired maximum amplitude value in the volume (a scaling will be done accordingly)");
        const Argument Argument::density_threshold("thresh", "t", "desired density threshold (partially thresholded if used with refinement)");
        
        const Argument Argument::number_of_beads("beads", "b", "number of beads to be used");
        
        const Argument Argument::refin("refin", "r", "reference mrc/map file");
        const Argument Argument::number_of_iterations("it", "i", "Number of iterations");
        const Argument Argument::membrane_slab("slab", "", "The membrane height in ratio of the Z length of the volume");
        const Argument Argument::temp_loc("temp", "", "Folder to keep temporary files for each iteration (if not specified temp files will not be written)");
        
        const Argument Argument::extended("extended", "", "Produce an output with 2X2X1 unit cells", false);
        const Argument Argument::inverted("inverted", "", "Produce an output map with inverted hand", false);
        const Argument Argument::psf("psf", "", "Produce an Point Spread Function(PSF) output map (works only with mrcout)", false);
        const Argument Argument::full_fourier("full-fourier", "", "Produce a full P1 Fourier space in output (noticeable only with hklout)", false);
        
        const Argument Argument::info("info", "", "Produces information of the input map", false);
        
        
    }
}

#endif	/* ARGUMENT_TEMPLATE_HPP */

