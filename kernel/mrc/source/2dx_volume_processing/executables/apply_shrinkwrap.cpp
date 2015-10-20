/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#include <iostream>
#include <fstream>
#include <string.h>
#include <math.h>

#include "../include/2dx_volume_processing.h"

/**
 * A function to refine volume using a reference mrc.
 * @param argc
 * @param argv
 * @return 
 */
int main(int argc, char** argv)
{
    
    args::Executable exe("A program to refine input map/mrc volume using shrinkwrap algorithm.", ' ', "1.0" );
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    args::templates::THRESHOLD.forceRequired();
    args::templates::ITERATIONS.forceRequired();
    
    //Add arguments  
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::HKLOUT);
    exe.add(args::templates::MASK_RES);
    exe.add(args::templates::THRESHOLD);
    exe.add(args::templates::ITERATIONS);
    exe.add(args::templates::MAXRES);
    exe.add(args::templates::SYMMETRY);
    exe.add(args::templates::TEMP_LOC);
    exe.add(args::templates::MRCIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Get and check the variables
    std::string mrcin = args::templates::MRCIN.getValue();
    std::string temp_loc = args::templates::TEMP_LOC.getValue();  
    std::string symmetry = args::templates::SYMMETRY.getValue();
    double max_resolution = args::templates::MAXRES.getValue();
    double density_threshold = args::templates::THRESHOLD.getValue();
    int number_of_iterations = args::templates::ITERATIONS.getValue();  
    std::string hklout = args::templates::HKLOUT.getValue();
    std::string mrcout = args::templates::MRCOUT.getValue();
    
    double mask_resolution = 15.0;
    if(args::templates::MASK_RES.isSet())  mask_resolution = args::templates::MASK_RES.getValue();
    
    if(!(args::templates::HKLOUT.isSet()) && !(args::templates::MRCOUT.isSet()))
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout!\n";
        std::cerr << "\nFor full details type:\n\t" << exe.getProgramName() << " --help \n\n\n";
        exit(1);
    }
    
    std::cout << "\n-----------------------------------\n";
    std::cout << ":Preparing the input volume:\n";
    std::cout << "-----------------------------------\n\n";
    Volume2dx input_volume;
    input_volume.read_volume(mrcin);
    input_volume.set_symmetry(symmetry);
    if(args::templates::MAXRES.isSet()) input_volume.low_pass(args::templates::MAXRES.getValue()); 
    input_volume.rescale_to_max_amplitude(10000);
    input_volume.prepare_fourier();
    input_volume.prepare_real();
    std::cout << input_volume.to_string();
 
    //Prepare the lowpassed binary mask for the shrinkwrap algorithm
    Volume2dx mask(input_volume);
    
    //Prepare the output volume
    Volume2dx output_volume(input_volume);
    
    double error = 1.0;
    for(int iteration=0; iteration<number_of_iterations; ++iteration)
    {
        
        std::string iteration_str = std::to_string(iteration+1);
        
        //Put leading zeros in front of iteration number
        for(int i=0; i<6-iteration_str.length(); i++)
        {
            iteration_str = "0" + iteration_str;
        }
        
        std::cout << "\n-----------------------------------\n";
        std::cout << "::Shrinkwrap Iteration: " << iteration+1 << std::endl;
        std::cout << "-----------------------------------\n";
        
        //Replace the reflections from that of input
        output_volume.replace_reflections(input_volume.get_fourier());
        
        if(temp_loc != "")
        {   
            std::string out_file_name = temp_loc + "/shrinkwrap_initial_volume_" + iteration_str +".map";
            output_volume.write_volume(out_file_name, "map");
        }
        
        //Prepare for constraints
        volume::data::RealSpaceData real_before(output_volume.get_real());
        
        //Get the sum of densities for error calculation
        double sum_initial = real_before.squared_sum();
        
        volume::data::RealSpaceData mask_threshold = real_before.threshold_mask(0);
        real_before.apply_mask(mask_threshold);
        
        //volume::data::RealSpaceData mask_slab = real_before.vertical_slab_mask(membrane_slab, true);
        //real_before.apply_mask(mask_slab);
        
        mask.set_real(real_before);
        mask.low_pass(mask_resolution);
        if(temp_loc != "") mask.write_volume(temp_loc+ "/mask_volume_shrinkwrap_" + iteration_str +".map");
    
        volume::data::RealSpaceData mask_shrinkwrap = mask.get_real().threshold_mask(density_threshold*real_before.max()/100);
    
        //Just to write output of mask to file
        mask.set_real(mask_shrinkwrap);
        if(temp_loc != "") mask.write_volume(temp_loc+ "/mask_binary_shrinkwrap_" + iteration_str +".map");
        
        //Mask the volume
        volume::data::RealSpaceData real_after = real_before.mask_applied_data(mask_shrinkwrap, 1.0);
        
        //Get the sum of densities for error calculation
        double sum_final = real_after.squared_sum();
        double itr_error = ((sum_initial-sum_final)/sum_initial);
        
        //Begin with setting a real space volume
        output_volume.set_real(real_after);
        
        //Low pass filter to use whatever is required
        output_volume.low_pass(max_resolution);
        
        std::cout << "\nIteration result:\n";
        std::cout << "\n:Squared Error = " << itr_error;
        if(iteration != 0) std::cout << " (Change = " << error-itr_error << ")\n\n";
        else std::cout << "\n\n";
        
        //Check for convergence
        if( (error - itr_error < 0.0000000001) || itr_error < 0.01)
        //if(itr_error < 0.01)
        {
            std::cout << ":\n\nConvergence criterion found after iteration: " << iteration+1 <<"\n";
            std::cout << "Stopping iterations\n\n\n";
            break;
        }
        
        if(temp_loc != "")
        {   
            std::string out_file_name = temp_loc + "/shrinkwrap_final_volume_" + iteration_str +".map";
            output_volume.write_volume(out_file_name, "map");
        }
        
        
        //Done with this iteration.
        //Prepare to write output
        //output_volume.prepare_fourier();
        //output_volume.prepare_real();
        //std::cout << output_volume.data_string();
        
        
        error = itr_error;
    }
    
    std::cout << "\n::Done with the iterations.\n";
    
    //Resolution limit
    output_volume.low_pass(max_resolution);

    //Symmetrize
    output_volume.symmetrize();

    //Prepare real/Fourier volumes
    output_volume.prepare_fourier();
    output_volume.prepare_real();
    
    std::cout << "\n-----------------------------------\n";
    std::cout << ":Final results:\n";
    std::cout << "-----------------------------------\n";
    std::cout << output_volume.to_string();

    //Write output in HKL format
    if(hklout != "") output_volume.write_volume(hklout, "hkl");
    if(mrcout != "") output_volume.write_volume(mrcout);
    
    return 0;
    
}


