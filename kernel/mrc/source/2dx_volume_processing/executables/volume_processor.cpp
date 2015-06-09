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
 * A Universal volume processor. 
 */
int main(int ac, char* av[]) 
{
    
    std::string prog_help = "A universal 2D crystallography volume processor.";
    
    //Parse the arguments
    std::vector<args::Argument> program_args = 
        {args::Argument::hklin, args::Argument::hkzin, args::Argument::mrcin, args::Argument::nx, args::Argument::ny, args::Argument::nz, args::Argument::gamma,
         args::Argument::symmetry, args::Argument::max_resolution, args::Argument::max_amplitude, args::Argument::density_threshold,
         args::Argument::hklout, args::Argument::mrcout, 
         args::Argument::extended, args::Argument::inverted, args::Argument::psf, args::Argument::full_fourier};
    args::ArgumentParser parser(program_args, ac, av, prog_help);
    
    //Get and check the variables
    std::cout << "::Reading arguments from command line:\n";
    std::string hklin = parser.get(args::Argument::hklin);
    std::string hkzin = parser.get(args::Argument::hkzin);
    std::string mrcin = parser.get(args::Argument::mrcin);
    int nx = parser.get_int(args::Argument::nx);
    int ny = parser.get_int(args::Argument::ny);
    int nz = parser.get_int(args::Argument::nz);
    double gamma = parser.get_double(args::Argument::gamma);
    std::string symmetry = parser.get(args::Argument::symmetry);
    double max_resolution = parser.get_double(args::Argument::max_resolution);
    double max_amplitude = parser.get_double(args::Argument::max_amplitude);
    double density_threshold = parser.get_double(args::Argument::density_threshold);
    
    std::string hklout = parser.get(args::Argument::hklout);
    std::string mrcout = parser.get(args::Argument::mrcout);
    
    bool extended = parser.get_bool(args::Argument::extended);
    bool inverted = parser.get_bool(args::Argument::inverted);
    bool psf = parser.get_bool(args::Argument::psf);
    bool full_fourier = parser.get_bool(args::Argument::full_fourier);
    
    if((hklin != "" || hkzin != "") && (nx == 0 || ny==0 || nz==0))
    {
        std::cerr << "\n\nERROR: Please specify nx, ny, nz with the option hklin/hkzin!\n";
        std::cerr << parser;
        exit(1);
    }
    
    if(hklin == "" && mrcin == "" && hkzin == "")
    {
        std::cerr << "\n\nERROR: Please specify at least one input with hklin or hkzin or mrcin!\n";
        std::cerr << parser;
        exit(1);
    }
    
    if( (hklin != "" && mrcin != "") || (hkzin != "" && mrcin != "") || (hklin != "" && hkzin != ""))
    {
        std::cerr << "\n\nERROR: Please specify only one input: hklin or hkzin or mrcin!\n";
        std::cerr << parser;
        exit(1);
    }
    
    if(hklout == "" && mrcout == "")
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout!\n";
        std::cerr << parser;
        exit(1);
    }
    
    //Prepare the input
    Volume2dx input(nx, ny, nz);
    if(gamma != 0) input.set_gamma_degrees(gamma);
    if(max_resolution != 0) input.set_max_resolution(max_resolution);
    //Read data
    if(hkzin != "")
    {
        input.read_volume(hkzin, "hkz");
    }
    else if(hklin != "")
    {
        input.read_volume(hklin, "hkl");
    }
    else if(mrcin != "")
    {
        input.read_volume(mrcin);
    }
    
    if(gamma != 0) input.set_gamma_degrees(gamma);
    if(symmetry != "")
    {
        input.set_symmetry(symmetry);
        input.symmetrize();
    }
    
    if(max_amplitude != 0.0) input.rescale_to_max_amplitude(max_amplitude);
    if(density_threshold != 0.0 ) input.apply_density_threshold(density_threshold);
    
    if(extended) input = input.extended_volume(1,1,0);
    if(inverted) input.invert_hand();
    
    if(psf)
    {
        std::cout << "Creating PSF\n";
        input = input.zero_phases();
        input.centerize_density_along_xyz();
        input.grey_scale_densities();
    }
    
    if(full_fourier) input.extend_to_full_fourier();
    
    if(hklout != "") input.write_volume(hklout, "hkl");
    if(mrcout != "") input.write_volume(mrcout);
    
    return 0;
}

