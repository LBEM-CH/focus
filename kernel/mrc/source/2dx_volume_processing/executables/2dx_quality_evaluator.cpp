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
 * Quality measures include phase residuals
 */
int main(int argc, char* argv[]) 
{
    args::Executable exe("A script to measure quality of the merged data using phase residuals.", ' ', "1.0" );
    
    //Custom arguments
    TCLAP::ValueArg<std::string> RESIDUALS("", "residuals", "Output file to be used for writing phase residuals", false, "","FILE");
    
    //Forced arguments
    args::templates::NX.forceRequired();
    args::templates::NY.forceRequired();
    args::templates::NZ.forceRequired();
    args::templates::GAMMA.forceRequired();
    args::templates::HKZIN.forceRequired();

    
    //Add arguments from templates
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
    
    bool initialized = false;
    tdx::data::MillerIndex current_index;
    tdx::data::PeakList current_spots;
    tdx::data::ResolutionBinnedData binnedPR(0, 1/resolution_max, 50);
    
    for(tdx::data::MillerToPeakMultiMap::const_iterator spot_itr=peak_multimap.begin(); spot_itr!=peak_multimap.end(); ++spot_itr)
    {
        // Initialize for the start
        if(!(initialized))
        {
            current_index = (*spot_itr).first;
            initialized = true;
        }
        
        // Average and insert accumulated spots
        if(!(current_index == (*spot_itr).first))
        {
            double resolution = tdx::utilities::fourier_utilities::get_resolution(current_index, gamma, a, b, c);
            double residual = tdx::utilities::fourier_utilities::calculate_phase_residual(current_spots);
            // std::cout << current_index.to_string() << " " << resolution << " " << residual << "\n";
            binnedPR.add_data_at(1/resolution, residual);
            current_spots.clear();
        }
        
        //Accumulate the spots in a list
        current_spots.push_back((*spot_itr).second);
        current_index = (*spot_itr).first;
        
    }
    
    //Write last data point
    double resolution = tdx::utilities::fourier_utilities::get_resolution(current_index, gamma, a, b, c);
    double residual = tdx::utilities::fourier_utilities::calculate_phase_residual(current_spots);
    binnedPR.add_data_at(resolution, residual);
    
    //Write out binned data
    binnedPR.write_average(RESIDUALS.getValue());
    std::cout << "Averaged binned phase residuals:\n";
    std::cout << binnedPR.plot_average();
    
    return 0;
}

