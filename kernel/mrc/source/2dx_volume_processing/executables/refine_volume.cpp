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
    std::string prog_help = "A program to refine input map/mrc volume.";
    
    //Parse the arguments
    std::vector<args::Argument> program_args = 
        {args::Argument::mrcin, args::Argument::refin, args::Argument::temp_loc, args::Argument::symmetry, args::Argument::max_resolution, args::Argument::density_threshold,
         args::Argument::number_of_iterations, args::Argument::membrane_slab,
         args::Argument::hklout, args::Argument::mrcout};
    args::ArgumentParser parser(program_args, argc, argv, prog_help);
    
    //Get and check the variables
    std::cout << "::Reading arguments from command line:\n";
    std::string mrcin = parser.get(args::Argument::mrcin, true);
    std::string refin = parser.get(args::Argument::refin, true);
    std::string temp_loc = parser.get(args::Argument::temp_loc);
    
    std::string symmetry = parser.get(args::Argument::symmetry);
    double max_resolution = parser.get_double(args::Argument::max_resolution);
    double density_threshold = parser.get_double(args::Argument::density_threshold, true);
    
    int number_of_iterations = parser.get_int(args::Argument::number_of_iterations, true);
    double membrane_slab = parser.get_double(args::Argument::membrane_slab, true);
    
    std::string hklout = parser.get(args::Argument::hklout);
    std::string mrcout = parser.get(args::Argument::mrcout);
    
    if(hklout == "" && mrcout == "")
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout!\n";
        std::cerr << parser;
        exit(1);
    }
    
    std::cout << "\n-----------------------------------\n";
    std::cout << "Preparing the input volume:\n";
    std::cout << "-----------------------------------\n\n";
    Volume2dx input_volume;
    input_volume.set_symmetry(symmetry);
    input_volume.set_max_resolution(max_resolution);
    input_volume.read_volume(mrcin);
    input_volume.prepare_fourier();
    std::cout << input_volume.to_string();
    
    std::cout << "\n-----------------------------------\n";
    std::cout << "Preparing the reference volume: \n";
    std::cout << "-----------------------------------\n\n";
    Volume2dx ref_volume;
    ref_volume.read_volume(refin);
    std ::cout << ref_volume.to_string();
    
    //Calculate the SF from ref volume
    volume::data::StructureFactors ref_structure_factors = ref_volume.calculate_structure_factors(100);
    if(temp_loc != "")
    {
        std::ofstream ref_sf_file(temp_loc+"/ref_sf.dat");
        ref_sf_file << ref_structure_factors.to_string();
        ref_sf_file.close();
    }
 
    std::cout << "\nReference Structure factors:\n";
    std::cout << ref_structure_factors.plot_profile();
    
    Volume2dx output_volume(input_volume.header());
    output_volume.set_data(input_volume);
    
    for(int iteration=0; iteration<number_of_iterations; ++iteration)
    {
        std::cout << "\n-----------------------------------\n";
        std::cout << "::Iteration: " << iteration+1 << std::endl;
        std::cout << "-----------------------------------\n";
        
        //Replace the reflection from that of input
        output_volume.replace_reflections(input_volume.get_fourier(), 0.5);
        
        //Apply structure factors
        output_volume.apply_structure_factors(ref_structure_factors, 0.5);
        
        //Apply low pass filter
        output_volume.low_pass(max_resolution);
        
        //Apply membrane slab
        output_volume.apply_density_slab(membrane_slab, 0.1, true);
        
        //Apply density threshold
        output_volume.apply_density_threshold(density_threshold, 0.1);
        
        //Apply density histogram
        output_volume.apply_density_histogram(ref_volume, 0.5);
        
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
    
    //Symmetrize
    output_volume.symmetrize();

    //Prepare real/Fourier volumes
    output_volume.prepare_fourier();
    output_volume.prepare_real();
    
    std::cout << "\n-----------------------------------\n";
    std::cout << "Final results:\n";
    std::cout << "-----------------------------------\n";
    std::cout << output_volume.to_string();
    std::cout << "\nNew structure factor profile:\n";
    std::cout << output_volume.calculate_structure_factors(100).plot_profile();
    

    //Write output in HKL format
    if(hklout != "") output_volume.write_volume(hklout, "hkl");
    if(mrcout != "") output_volume.write_volume(mrcout);
    
    return 0;
    
}


