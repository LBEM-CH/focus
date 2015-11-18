/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <string.h>

#include "../include/2dx_volume_processing.h"

int main(int argc, char* argv[])
{
    
    args::Executable exe("(NOT READY YET)Program to add Gaussian noise to mrc/map.", ' ', "1.0" );
    
    TCLAP::ValueArg<double> NOISE_AMOUNT("", "amount", "Amount of noise to be added (0, 1). Default is set to 0.2", false, 0.2,"FLOAT");
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    
    //Add arguments
    exe.add(args::templates::MRCIN);
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::HKLOUT);
    exe.add(NOISE_AMOUNT);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    std::string hklout = args::templates::HKLOUT.getValue();
    std::string mrcout = args::templates::MRCOUT.getValue();
    
    if(!(args::templates::HKLOUT.isSet()) && !(args::templates::MRCOUT.isSet()))
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout!\n";
        std::cerr << "\nFor full details type:\n\t" << exe.getProgramName() << " --help \n\n\n";
        exit(1);
    }
    
    double amount = 0.2;
    if(NOISE_AMOUNT.isSet()) amount = NOISE_AMOUNT.getValue(); 
    
    //Prepare the input
    Volume2dx input;
    input.read_volume(args::templates::MRCIN.getValue());
    input.grey_scale_densities();
    
    Volume2dx random_noise(input);
    random_noise.generate_fourier_noise(amount);
    //random_noise.generate_poisson_densities(input.get_real().squared_sum()*10);
    
    //input.grey_scale_densities();
    //random_noise.grey_scale_densities();
    
    //Volume2dx output = input + random_noise;
    
    std::cout << "SNR of output: " << input.get_fourier().intensity_sum() / random_noise.get_fourier().intensity_sum() << std::endl;
    
    //Write output in HKL format
    if(hklout != "") random_noise.write_volume(hklout, "hkl");
    if(mrcout != "") random_noise.write_volume(mrcout);
    
    return 0;
}
