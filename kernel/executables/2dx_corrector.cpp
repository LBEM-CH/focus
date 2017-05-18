/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#include <iostream>
#include <fstream>
#include <string.h>
#include <math.h>

#include "2dx_toolkit.h"

/**
 * A function to refine volume using a reference mrc.
 * @param argc
 * @param argv
 * @return 
 */
int main(int argc, char** argv)
{
    /********************************************
     * Command Line Setup
     *********************************************/
    args::Executable exe("A program to refine input map/mrc volume using shrinkwrap algorithm.", ' ', "1.0" );
    
    TCLAP::ValueArg<double> BIN_SIZE("", "bin_size", "Resolution bin size to correct in each iteration (in A)", false, 1.0, "FLOAT");
    TCLAP::ValueArg<int> ITERATIONS("", "iterations", "Maximum number of iterations for each resolution bin. Would stop if the phases changed are significantly lower.", false, 0,"INT");
    static TCLAP::ValueArg<double> THRESHOLDH("", "threshold_higher", "Higher density threshold for mask (as percentage)", true, -1.0,"FLOAT");
    static TCLAP::ValueArg<double> THRESHOLDL("", "threshold_lower", "Lower density threshold for mask (as percentage)", true, -1.0,"FLOAT");
    static TCLAP::ValueArg<double> MINRES("", "res_start", "The starting resolution for refinement", false, 2.0, "FLOAT");
    static TCLAP::ValueArg<double> MAXRES("", "res_max", "maximum expected resolution of the map (default 2.0)", false, 2.0, "FLOAT");
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    ITERATIONS.forceRequired();
    args::templates::MRCOUT.forceRequired();
    MINRES.forceRequired();
    MAXRES.forceRequired();
    
    //Add arguments  
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::SYMMETRY);
    exe.add(args::templates::TEMP_LOC);
    exe.add(BIN_SIZE);
    exe.add(args::templates::MASK_RES);
    exe.add(THRESHOLDL);
    exe.add(THRESHOLDH);
    exe.add(MAXRES);
    exe.add(MINRES);
    exe.add(ITERATIONS);
    exe.add(args::templates::MRCIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Get and check the variables
    std::string mrcin = args::templates::MRCIN.getValue();
    std::string temp_loc = args::templates::TEMP_LOC.getValue();  
    std::string symmetry = args::templates::SYMMETRY.getValue();
    double max_resolution = MAXRES.getValue();
    double start_resolution = MINRES.getValue();
    int number_of_iterations = ITERATIONS.getValue();  
    std::string mrcout = args::templates::MRCOUT.getValue();
    double bin_size = BIN_SIZE.getValue();
    
    double density_threshold_higher = THRESHOLDH.getValue();
    double density_threshold_lower = THRESHOLDL.getValue();
    
    double mask_resolution = 15.0;
    if(args::templates::MASK_RES.isSet())  mask_resolution = args::templates::MASK_RES.getValue();
   
    std::cout << "\n-----------------------------------\n";
    std::cout << ":Preparing the input volume:\n";
    std::cout << "-----------------------------------\n\n";
    Volume2DX input_volume;
    input_volume.read_volume(mrcin);
    input_volume.set_symmetry(symmetry);
    input_volume.low_pass(max_resolution); 
    input_volume.rescale_to_max_amplitude(10000);
    input_volume.prepare_fourier();
    input_volume.prepare_real();
    std::cout << input_volume.to_string();
    
    /********************************************
     * Shrinkwrap Iterations
     *********************************************/
    
    int res_itr = 0;
    for(int res=start_resolution; res>max_resolution; res=res-bin_size) 
    {
        res_itr++;
        double res_begin = res;
        double res_end = res - bin_size;
        std::cout << "\n********************************************\n";
        std::cout << "::Refining in range: " << res_begin << "A - " << res_end << "A\n";
        std::cout << "\n********************************************\n";
        
        Volume2DX start_volume(input_volume);
        start_volume.low_pass(res_end);
        
        Volume2DX iteration_volume(start_volume);
        Volume2DX mask(start_volume);
        
        int phase_changes_itr = 0;
        int phase_changes_last = 0;
        
        for(int iteration=0; iteration<number_of_iterations; ++iteration)
        {

            std::string iteration_str = std::to_string(iteration+1);

            //Put leading zeros in front of iteration number
            for(int i=0; i<6-iteration_str.length(); i++) iteration_str = "0" + iteration_str;
            iteration_str = std::to_string(res_itr) + "_" + iteration_str;

            std::cout << "\n-----------------------------------\n";
            std::cout << "::Shrinkwrap Iteration: " << iteration+1 << std::endl;
            std::cout << "-----------------------------------\n";

            /********************************************
            * Real Space constraints
            *********************************************/
            tdx::data::RealSpaceData real_space_data(iteration_volume.get_real());

            //Threshold mask
            tdx::data::RealSpaceData mask_threshold = real_space_data.threshold_mask(0);
            real_space_data.apply_mask(mask_threshold);

            //Shrinkwrap mask
            mask.set_real(real_space_data);
            mask.low_pass_butterworth(mask_resolution);
            if(temp_loc != "") mask.write_volume(temp_loc+ "/mask_volume_" + iteration_str +".map");
            double maxDensity = real_space_data.max();
            tdx::data::RealSpaceData mask_shrinkwrap = mask.get_real().threshold_soft_mask(density_threshold_higher*maxDensity/100, density_threshold_lower*maxDensity/100);

            //Just to write output of mask to file
            mask.set_real(mask_shrinkwrap);
            if(temp_loc != "") mask.write_volume(temp_loc+ "/mask_binary_" + iteration_str +".map");

            real_space_data.multiply_mask(mask_shrinkwrap);

            //Finally set the modified real space volume
            iteration_volume.set_real(real_space_data);

            
            /********************************************
            * Fourier Space constraints
            *********************************************/

            //Low pass filter to use whatever is required
            iteration_volume.low_pass(res_end);

            //Check for the changes in the phases
            tdx::data::ReflectionData fourier_itr_data = iteration_volume.get_fourier();
            tdx::data::ReflectionData corrected_data(fourier_itr_data);
            
            phase_changes_itr = 0;
            for(const auto& data: fourier_itr_data) {
                int h = data.first.h();
                int k = data.first.k();
                int l = data.first.l();
                double spot_resolution = iteration_volume.resolution_at(h, k, l);
                if(spot_resolution <= res_begin && spot_resolution > res_end) {
                    double orig_phase = start_volume.get_fourier().value_at(h, k, l).phase();
                    double orig_alt_phase = orig_phase + M_PI;
                    double itr_phase = data.second.phase();
                    double new_phase;
                    
                    if( abs(itr_phase-orig_phase) < abs(itr_phase - orig_alt_phase) ) {
                        new_phase = orig_phase;
                    } else {
                        new_phase = orig_alt_phase;
                        phase_changes_itr++;
                    }
                    
                    double amp = data.second.amplitude();
                    tdx::Complex new_complex(amp*cos(new_phase), amp*sin(new_phase));
                    
                    corrected_data.set_spot_at(h, k, l, new_complex, data.second.weight());
                }
            }
            
            iteration_volume.set_fourier(corrected_data);
            
            int itr_change = phase_changes_last - phase_changes_itr; 
            
            std::cout << "Phases swapped in this iteration: " << phase_changes_itr << std::endl;
            std::cout << "Phase swap changes from last iteration: " << itr_change << std::endl;
            
            if(itr_change < 5) {
                std::cout << "CONVERGED: Terminating this iteration.\n";
            }
            
            phase_changes_last = phase_changes_itr;

            if(temp_loc != "") iteration_volume.write_volume(temp_loc + "/final_volume_" + iteration_str +".map", "map");
        }
        
        std::cout << "\n::Done with the iterations.\n";
        
        //Change the input volume to put in the corrected reflections
        tdx::data::ReflectionData corrected_data = iteration_volume.get_fourier();
        tdx::data::ReflectionData new_input_data = input_volume.get_fourier();
        
        for(const auto& data : corrected_data) {
            int h = data.first.h();
            int k = data.first.k();
            int l = data.first.l();
            new_input_data.set_spot_at(h, k, l, data.second.value(), data.second.weight());
        }
        
        input_volume.set_fourier(new_input_data);
        
    }
    
    std::cout << "\n::Done with the refinement.\n";

    //Symmetrize
    input_volume.symmetrize();

    //Prepare real/Fourier volumes
    input_volume.prepare_fourier();
    input_volume.prepare_real();
    
    std::cout << "\n-----------------------------------\n";
    std::cout << ":Final results:\n";
    std::cout << "-----------------------------------\n";
    std::cout << input_volume.to_string();

    //Write output
    if(mrcout != "") input_volume.write_volume(mrcout);
    
    return 0;
    
    
    
}

