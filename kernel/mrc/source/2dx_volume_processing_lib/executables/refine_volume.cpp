/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#include <iostream>
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
    //Read the program inputs:
    //<prog_name> hkz_file symmetry nx ny nz gamma max_resolution output_file
    
    if (argc < 7) 
    {
        std::cout << "Program Options\n\t<input_mrc_file> <reference_mrc_file> <symmetry> <max_resolution> <iterations> <output_mrc_file>\n";
        return(1);
    }
    
    //Parse the inputs
    std::string input_volume_file = argv[1];
    std::string ref_volume_file = argv[2];
    std::string symmetry = argv[3];
    double max_resolution = std::atof(argv[4]);
    int iterations = std::atoi(argv[5]);
    std::string output_mrc = argv[6];
    
    std::cout << "\n-----------------------------------\n";
    std::cout << "Preparing the input volume:\n";
    std::cout << "-----------------------------------\n\n";
    Volume2dx input_volume;
    input_volume.set_symmetry(symmetry);
    input_volume.set_max_resolution(max_resolution);
    input_volume.read_volume(input_volume_file, "mrc");
    std::cout << input_volume.to_string();
    
    std::cout << "\n-----------------------------------\n";
    std::cout << "Preparing the reference volume: \n";
    std::cout << "-----------------------------------\n\n";
    Volume2dx ref_volume;
    ref_volume.set_max_resolution(max_resolution);
    ref_volume.read_volume(ref_volume_file, "mrc");
    std ::cout << ref_volume.to_string();
    
    //Calculate the sf from ref volume
    volume::data::StructureFactors structure_factors = ref_volume.calculate_structure_factors(100);
    std::cout << "\nReference Structure factors:\n";
    std::cout << structure_factors.plot_profile();
    
    Volume2dx output_volume(input_volume);
    
    for(int iteration=0; iteration<iterations; ++iteration)
    {
        std::cout << "\n-----------------------------------\n";
        std::cout << "\tIteration: " << iteration+1 << std::endl;
        std::cout << "-----------------------------------\n";
        
        //Apply structure factors
        std::cout << "\nCurrent structure factor profile:\n";
        output_volume.calculate_structure_factors(100).plot_profile();
        output_volume.apply_structure_factors(structure_factors, 0.5);
        std::cout << "\nNew structure factor profile:\n";
        std::cout << output_volume.calculate_structure_factors(100).plot_profile();
        
        //Apply density histogram
        output_volume.apply_density_histogram(ref_volume, 1.0);
    }
    
    std::cout << "\nDone with the iterations.\n";
    
    //Write the output volume to a file
    output_volume.write_volume(output_mrc, "mrc");
    
    
}


