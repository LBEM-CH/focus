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
    
    if (argc < 15) 
    {
        std::cout << "Program Options\n\t<input_hkz_file> <nx> <ny> <nz> <symmetry> <gamma> <max_amplitude> <max_resolution> <refinement_iterations> <density_threshold_refinement> <no_of_beads> <density_threshold_baed> <bead_model_resolution> <membrane_height_fraction>\n";
        return(1);
    }
    
    //Parse the inputs
    std::string input_hkz_file = argv[1];
    int nx = std::atoi(argv[2]);
    int ny = std::atoi(argv[3]);
    int nz = std::atoi(argv[4]);
    std::string symmetry = argv[5];
    double gamma = std::atof(argv[6]);
    double max_amplitude = std::atof(argv[7]);
    double max_resolution = std::atof(argv[8]);
    int iterations = std::atoi(argv[9]);
    double density_threshold_refinement = std::atof(argv[10]);
    int no_of_beads = std::atoi(argv[11]);
    double density_threshold_bead = std::atof(argv[12]);
    double bead_model_resolution = std::atof(argv[13]);
    double membrane_slab = std::atof(argv[14]);
    
     
    std::cout << "\n-----------------------------------\n";
    std::cout << "::Preparing the input volume:\n";
    std::cout << "-----------------------------------\n\n";
    Volume2dx input_volume(nx, ny, nz);
    input_volume.set_gamma(volume::utilities::angle_utilities::DegreeToRadian(gamma));
    input_volume.set_max_resolution(max_resolution);
    input_volume.set_symmetry(symmetry);
    input_volume.read_volume(input_hkz_file, "hkz");
    input_volume.rescale_to_max_amplitude(max_amplitude);
    input_volume.symmetrize();
    input_volume.prepare_real();
    std::cout << input_volume.to_string();
    
    //Write out the volume
    input_volume.write_volume("back_projected.hkl", "hkl");
    input_volume.write_volume("back_projected.map", "map");
    
    
    std::cout << "\n-----------------------------------\n";
    std::cout << "::Preparing the bead model volume: \n";
    std::cout << "-----------------------------------\n\n";
    Volume2dx bead_model_volume = input_volume.generate_bead_model(no_of_beads, density_threshold_bead, bead_model_resolution);
    std ::cout << bead_model_volume.to_string();
    
    //Write out the bead model
    bead_model_volume.write_volume("bead_model.hkl", "hkl");
    bead_model_volume.write_volume("bead_model.map", "map");
    
    //Calculate the sf from ref volume
    volume::data::StructureFactors ref_structure_factors = bead_model_volume.calculate_structure_factors(100);
    std::cout << "\nReference Structure factors:\n";
    std::cout << ref_structure_factors.plot_profile();
    
    Volume2dx output_volume(input_volume.header());
    output_volume.set_data(input_volume);
    
    for(int iteration=0; iteration<iterations; ++iteration)
    {
        std::cout << "\n-----------------------------------\n";
        std::cout << "::Iteration: " << iteration+1 << std::endl;
        std::cout << "-----------------------------------\n";
        
        //Replace the reflection from that of input
        output_volume.replace_reflections(input_volume.get_fourier(), 0.33);
        
        //Apply structure factors
        output_volume.apply_structure_factors(ref_structure_factors, 0.5);
        
        //Apply low pass filter
        output_volume.low_pass(max_resolution);
        
        //Apply membrane slab
        output_volume.apply_density_slab(membrane_slab, 0.1, true);
        
        //Apply density threshold
        output_volume.apply_density_threshold(density_threshold_refinement, 0.1);
        
        //Apply density histogram
        output_volume.apply_density_histogram(bead_model_volume, 0.5);
            
        
        
        //Done with this iteration.
        //Prepare to write output
        output_volume.prepare_fourier();
        output_volume.prepare_real();

        std::cout << "\nIteration result:\n";
        std::cout << output_volume.to_string();
        
        std::string out_file_name = "refined_" + std::to_string(iteration+1) +".map";
        output_volume.write_volume(out_file_name, "map");
               
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
    output_volume.write_volume("refined_final.hkl", "hkl");
    output_volume.write_volume("refined_final.map", "map");
    
    
}


