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
    /********************************************
     * Command Line Setup
     *********************************************/
    args::Executable exe("A program to refine input map/mrc volume using shrinkwrap algorithm.", ' ', "1.0" );
    
    TCLAP::ValueArg<double> CONE("", "cone", "(in degrees) The data in Fourier space will be filled only in the cone with specified degrees", false, 90.0, "FLOAT");
    TCLAP::ValueArg<double> AMP_CUTOFF("", "amp_cutoff", "All the reflections below this value would be ignored", false, 0.0, "FLOAT");
    static TCLAP::ValueArg<double> THRESHOLDH("", "threshold_higher", "Higher density threshold for mask (as percentage)", true, -1.0,"FLOAT");
    static TCLAP::ValueArg<double> THRESHOLDL("", "threshold_lower", "Lower density threshold for mask (as percentage)", true, -1.0,"FLOAT");
    static TCLAP::ValueArg<double> SCALE_CONE_ENERGY("", "scale_cone_energy", "Scales the calculated optimum cone energy in each iteration", true, 1.0,"FLOAT");
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    args::templates::ITERATIONS.forceRequired();
    
    //Add arguments  
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::HKLOUT);
    exe.add(args::templates::MAXRES);
    exe.add(args::templates::SYMMETRY);
    exe.add(args::templates::TEMP_LOC);
    exe.add(AMP_CUTOFF);
    exe.add(SCALE_CONE_ENERGY);
    exe.add(CONE);
    exe.add(args::templates::SLAB);
    exe.add(args::templates::MASK_RES);
    exe.add(THRESHOLDL);
    exe.add(THRESHOLDH);
    exe.add(args::templates::ITERATIONS);
    exe.add(args::templates::MRCIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Get and check the variables
    std::string mrcin = args::templates::MRCIN.getValue();
    std::string temp_loc = args::templates::TEMP_LOC.getValue();  
    std::string symmetry = args::templates::SYMMETRY.getValue();
    double max_resolution = args::templates::MAXRES.getValue();
    int number_of_iterations = args::templates::ITERATIONS.getValue();  
    std::string hklout = args::templates::HKLOUT.getValue();
    std::string mrcout = args::templates::MRCOUT.getValue();
    double cone_angle = CONE.getValue();
    double amp_cutoff = AMP_CUTOFF.getValue();
    double scale_cone_energy = SCALE_CONE_ENERGY.getValue();
    
    double density_threshold_higher = THRESHOLDH.getValue();
    double density_threshold_lower = THRESHOLDL.getValue();
    
    double membrane_slab = 1.0;
    if(args::templates::SLAB.isSet()) membrane_slab = args::templates::SLAB.getValue();
    
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
    
    /********************************************
     * Shrinkwrap Iterations
     *********************************************/
    
    //Initialize iteration volume and mask   
    Volume2dx iteration_volume(input_volume);
    Volume2dx iteration_cone;
    Volume2dx iteration_without_cone;
    Volume2dx mask(input_volume);
    
    iteration_volume.cut_cone(iteration_cone, iteration_without_cone, cone_angle);
    double cone_energy_initial = iteration_cone.get_fourier().intensity_sum();
    // double optimum_cone_energy = iteration_without_cone.get_fourier().intensity_sum()*(cone_angle/(90-cone_angle));
    double optimum_cone_energy = iteration_without_cone.get_fourier().intensity_sum()*( (1-cos(cone_angle*M_PI/180.0))/cos(cone_angle*M_PI/180.0));
    std::cout << ":Optimum cone energy = " << optimum_cone_energy <<"\n";
    
    for(int iteration=0; iteration<number_of_iterations; ++iteration)
    {
        
        std::string iteration_str = std::to_string(iteration+1);
        
        //Put leading zeros in front of iteration number
        for(int i=0; i<6-iteration_str.length(); i++) iteration_str = "0" + iteration_str;
        
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
        
        //Slab mask
        tdx::data::RealSpaceData mask_slab = real_space_data.vertical_slab_mask(membrane_slab, true);
        real_space_data.apply_mask(mask_slab);
        
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
        iteration_volume.low_pass(max_resolution);

        //Cut the cone
        iteration_volume.cut_cone(iteration_cone, iteration_without_cone, cone_angle);
        double cone_energy_final = iteration_cone.get_fourier().intensity_sum();
        double error = (cone_energy_initial - cone_energy_final)/cone_energy_initial;
        std::cout <<":Squared error: " << error <<"\n";
        cone_energy_initial = cone_energy_final;
        
        //Scale the intensities
        std::cout << ":Rescaling energy from: " << cone_energy_final << " to: " << scale_cone_energy*optimum_cone_energy <<"\n";
        iteration_cone.rescale_energy(scale_cone_energy*optimum_cone_energy);
        
        //Replace the reflections from that of input
        iteration_cone.replace_reflections(input_volume.get_fourier(), cone_angle, amp_cutoff);
        
        iteration_volume = iteration_cone;
        
        //Calculate the new intensity sum
        iteration_volume.cut_cone(iteration_cone, iteration_without_cone, cone_angle);
        cone_energy_initial = iteration_cone.get_fourier().intensity_sum();
               
        if(temp_loc != "") iteration_volume.write_volume(temp_loc + "/final_volume_" + iteration_str +".map", "map");
    }
    
    std::cout << "\n::Done with the iterations.\n";
    
    //Resolution limit
    iteration_volume.low_pass(max_resolution);

    //Symmetrize
    iteration_volume.symmetrize();

    //Prepare real/Fourier volumes
    iteration_volume.prepare_fourier();
    iteration_volume.prepare_real();
    
    std::cout << "\n-----------------------------------\n";
    std::cout << ":Final results:\n";
    std::cout << "-----------------------------------\n";
    std::cout << iteration_volume.to_string();

    //Write output in HKL format
    if(hklout != "") iteration_volume.write_volume(hklout, "hkl");
    if(mrcout != "") iteration_volume.write_volume(mrcout);
    
    return 0;
    
}


