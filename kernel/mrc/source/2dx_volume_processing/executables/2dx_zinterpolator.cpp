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
 * A z* to l interpolator 
 */
int main(int argc, char* argv[]) 
{
    args::Executable exe("Interpolates data from h,k,z* to equidistant vertical grid h,k,l using averaging", ' ', "1.0" );
    
    //Custom arguments
    TCLAP::ValueArg<std::string> RESIDUALS("", "residuals", "Output file to be used for writing phase residuals", false, "","FILE");
    
    //Forced arguments
    args::templates::NX.forceRequired();
    args::templates::NY.forceRequired();
    args::templates::NZ.forceRequired();
    args::templates::GAMMA.forceRequired();
    args::templates::HKZIN.forceRequired();

    
    //Add arguments from templates
    exe.add(RESIDUALS);
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::HKLOUT);
    exe.add(args::templates::SYMMETRY);
    exe.add(args::templates::GAMMA);
    exe.add(args::templates::NZ);
    exe.add(args::templates::NY);
    exe.add(args::templates::NX);
    exe.add(args::templates::HKZIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Get and check the variables
    std::string infile = args::templates::HKZIN.getValue();
    std::string informat = "hkz";
    
    if(!(args::templates::HKLOUT.isSet()) && !(args::templates::MRCOUT.isSet()))
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout!\n";
        std::cerr << "\nFor full details type:\n\t" << exe.getProgramName() << " --help \n\n\n";
        exit(1);
    }
    
    //Prepare the input
    Volume2dx input((int)args::templates::NX.getValue(), (int)args::templates::NY.getValue(), (int)args::templates::NZ.getValue());
    input.set_gamma_degrees(args::templates::GAMMA.getValue());
    
    //Read and set data
    input.read_volume(infile, informat);
    
    //Symmetrize
    if(args::templates::SYMMETRY.isSet())
    {
        input.set_symmetry(args::templates::SYMMETRY.getValue());
        input.symmetrize();
    }
    
    if(args::templates::HKLOUT.isSet()) input.write_volume(args::templates::HKLOUT.getValue(), "hkl");
    if(args::templates::MRCOUT.isSet()) input.write_volume(args::templates::MRCOUT.getValue());
    
    return 0;
}

