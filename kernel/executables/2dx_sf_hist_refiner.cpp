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
    
    args::Executable exe("A program to sharpen input map/mrc volume using reference volume.", ' ', "1.0" );
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    args::templates::REFIN.forceRequired();
    args::templates::ITERATIONS.forceRequired();
    
    //Add arguments  
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::HKLOUT);
    exe.add(args::templates::ITERATIONS);
    exe.add(args::templates::MAXRES);
    exe.add(args::templates::SYMMETRY);
    exe.add(args::templates::TEMP_LOC);
    exe.add(args::templates::REFIN);
    exe.add(args::templates::MRCIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Get and check the variables
    std::string mrcin = args::templates::MRCIN.getValue();
    std::string refin = args::templates::REFIN.getValue();
    std::string temp_loc = args::templates::TEMP_LOC.getValue();  
    std::string symmetry = args::templates::SYMMETRY.getValue();
    double max_resolution = args::templates::MAXRES.getValue();  
    int number_of_iterations = args::templates::ITERATIONS.getValue(); 
    std::string hklout = args::templates::HKLOUT.getValue();
    std::string mrcout = args::templates::MRCOUT.getValue();
    
    if(!(args::templates::HKLOUT.isSet()) && !(args::templates::MRCOUT.isSet()))
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout!\n";
        std::cerr << "\nFor full details type:\n\t" << exe.getProgramName() << " --help \n\n\n";
        exit(1);
    }
    
    std::cout << "\n-----------------------------------\n";
    std::cout << ":Preparing the input volume:\n";
    std::cout << "-----------------------------------\n\n";
    Volume2DX input_volume;
    input_volume.read_volume(mrcin);
    input_volume.set_symmetry(symmetry);
    input_volume.prepare_fourier();
    input_volume.prepare_real();
    std::cout << input_volume.to_string();
    
    std::cout << "\n-----------------------------------\n";
    std::cout << ":Preparing the reference volume: \n";
    std::cout << "-----------------------------------\n\n";
    Volume2DX ref_volume;
    ref_volume.read_volume(refin);
    ref_volume.grey_scale_densities();
    std ::cout << ref_volume.to_string();
    
    //Calculate the SF from ref volume
    tdx::data::BinnedData ref_structure_factors = ref_volume.calculate_structure_factors(0, 1/max_resolution, 100);
    if(temp_loc != "") ref_structure_factors.write_average(temp_loc+"/ref_sf.dat");
 
    std::cout << "\nReference Structure factors:\n";
    std::cout << ref_structure_factors.plot_average();
    
    Volume2DX output_volume = input_volume;
    
    for(int iteration=0; iteration<number_of_iterations; ++iteration)
    {
        std::cout << "\n-----------------------------------\n";
        std::cout << "::Sharpening Iteration: " << iteration+1 << std::endl;
        std::cout << "-----------------------------------\n";
     
        //Apply structure factors
        output_volume.apply_structure_factors(ref_structure_factors, 1.0);
        
        //Apply density histogram
        output_volume.apply_density_histogram(ref_volume, 1.0);
        
        //Done with this iteration.
        //Prepare to write output
        output_volume.prepare_fourier();
        output_volume.prepare_real();

        std::cout << "\nIteration result:\n";
        std::cout << output_volume.data_string();
        
        if(temp_loc != "")
        {   
            std::string out_file_name = temp_loc + "/sharpened_" + std::to_string(iteration+1) +".map";
            output_volume.write_volume(out_file_name, "map");
        }
    }
    
    std::cout << "\n::Done with the iterations.\n";
    
    //Resolution limit
    output_volume.low_pass(max_resolution);

    //Symmetrize
    output_volume.symmetrize();
    
    output_volume.apply_density_threshold(0);
    output_volume.grey_scale_densities();

    //Prepare real/Fourier volumes
    output_volume.prepare_fourier();
    output_volume.prepare_real();
    
    std::cout << "\n-----------------------------------\n";
    std::cout << ":Final results:\n";
    std::cout << "-----------------------------------\n";
    std::cout << output_volume.to_string();
    std::cout << "\nNew structure factor profile:\n";
    std::cout << output_volume.calculate_structure_factors(0, 1/max_resolution, 100).plot_average();
    

    //Write output in HKL format
    if(hklout != "") output_volume.write_volume(hklout, "hkl");
    if(mrcout != "") output_volume.write_volume(mrcout);
    
    return 0;
    
}


