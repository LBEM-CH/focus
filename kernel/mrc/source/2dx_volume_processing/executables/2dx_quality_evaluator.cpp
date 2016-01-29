/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include "../include/2dx_volume_processing.h"

/*
 * Calculates qualtiy of merged data in 2dx.
 * Quality measures include phase residuals, fsc 
 */
int main(int argc, char* argv[]) 
{
    args::Executable exe("A script to measure quality of the merged data using phase residuals, FSC", ' ', "1.0" );
    
    //Custom arguments
    TCLAP::ValueArg<std::string> RESIDUALS("", "residuals", "Output file to be used for writing phase residuals", false, "","FILE");
    TCLAP::ValueArg<std::string> FSC("", "fsc", "Output file to be used for writing FSC", false, "","FILE");
    
    //Forced arguments
    args::templates::NX.forceRequired();
    args::templates::NY.forceRequired();
    args::templates::NZ.forceRequired();
    args::templates::GAMMA.forceRequired();
    args::templates::HKZIN.forceRequired();

    
    //Add arguments from templates
    exe.add(FSC);
    exe.add(RESIDUALS);
    exe.add(args::templates::MAXRES);
    exe.add(args::templates::GAMMA);
    exe.add(args::templates::NZ);
    exe.add(args::templates::NY);
    exe.add(args::templates::NX);
    exe.add(args::templates::HKZIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    double a = args::templates::NX.getValue();
    double b = args::templates::NY.getValue();
    double c = args::templates::NZ.getValue();
    double gamma = args::templates::GAMMA.getValue();
    std::string infile = args::templates::HKZIN.getValue();
    double resolution_max = 2.0;
    if(args::templates::MAXRES.isSet()) resolution_max = args::templates::MAXRES.getValue();
    
    //Read the data 
    tdx::data::MillerToPeakMultiMap peak_multimap;
    tdx::io::reflection::read(infile, (int)c, true, peak_multimap );
    
    
    //Calculate binned phase residuals
    if(RESIDUALS.isSet())
    {
        tdx::data::ResolutionBinnedData binnedPR(0, 1/resolution_max, 50);
        tdx::utilities::quality_evaluation::corrected_phase_residual(peak_multimap, a, b, c, gamma, binnedPR);
        
        //Write out binned data
        binnedPR.write_sum(RESIDUALS.getValue());
        std::cout << "\n\n----------------------------------\n";
        std::cout << "Averaged binned phase residuals:\n";
        std::cout << "----------------------------------\n";
        std::cout << binnedPR.plot_sum();
    }
    
    //Calculate binned FSC
    if(FSC.isSet())
    {
        tdx::data::ResolutionBinnedData binnedFSC(0, 1/resolution_max, 50);
        tdx::utilities::quality_evaluation::fourier_shell_correlation(peak_multimap, a, b, c, gamma, binnedFSC);
        
        //Write out binned data
        binnedFSC.write_sum(FSC.getValue());
        std::cout << "\n\n--------------------------\n";
        std::cout << "Averaged binned FSC:\n";
        std::cout << "--------------------------\n";
        std::cout << binnedFSC.plot_sum();
    }
    
    
    
    return 0;
}

