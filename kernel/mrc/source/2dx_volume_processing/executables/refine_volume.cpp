/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#include <iostream>
#include <fstream>
#include <string.h>

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
    args::templates::SLAB.forceRequired();
    
    //Add arguments  
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::HKLOUT);
    exe.add(args::templates::SLAB);
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
    double membrane_slab = args::templates::SLAB.getValue();  
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
    input_volume.apply_density_threshold(0);
    input_volume.grey_scale_densities();
    input_volume.prepare_fourier();
    input_volume.prepare_real();
    std::cout << input_volume.to_string();
 
    //Start from an input volume
    Volume2dx output_volume(input_volume.header());
    output_volume.set_data(input_volume);
    
    for(int iteration=0; iteration<number_of_iterations; ++iteration)
    {
        std::cout << "\n-----------------------------------\n";
        std::cout << "::Refinement Iteration: " << iteration+1 << std::endl;
        std::cout << "-----------------------------------\n";
        
        //Symmetrize to reduce the dimensionality of problem
        output_volume.symmetrize();
        
        //Replace the reflection from that of input
        output_volume.replace_reflections(input_volume.get_fourier(), 1.0);
        
        //Apply membrane slab
        output_volume.apply_density_slab(membrane_slab, 0.5, true);
        
        //Apply shrinkwrap
        Volume2dx mask(output_volume.header());
        mask.set_fourier(output_volume.get_fourier());
        mask.low_pass(mask_resolution);
        if(temp_loc != "") mask.write_volume(temp_loc+ "/mask_volume_iteration_" + std::to_string(iteration+1) + ".map", "map");
        
        double itr_threshold = density_threshold - iteration*density_threshold*(1/number_of_iterations);
        if(itr_threshold < 0) itr_threshold = 0;
        volume::data::RealSpaceData mask_real = mask.get_real().binary_mask(itr_threshold);
        mask.set_real(mask_real);
        if(temp_loc != "") mask.write_volume(temp_loc+ "/mask_binary_iteration_" + std::to_string(iteration+1) + ".map", "map");
        
        output_volume.apply_real_mask(mask_real, 0.2);
        
        //Rescale the densities/amplitudes
        output_volume.rescale_to_max_amplitude(10000);
        output_volume.apply_density_threshold(0);
        output_volume.grey_scale_densities();
      
        //Done with this iteration.
        //Prepare to write output
        output_volume.prepare_fourier();
        output_volume.prepare_real();

        std::cout << "\nIteration result:\n";
        std::cout << output_volume.data_string();
        
        if(temp_loc != "")
        {   
            std::string out_file_name = temp_loc + "/refined_" + std::to_string(iteration+1) +".map";
            output_volume.write_volume(out_file_name, "map");
        }
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


