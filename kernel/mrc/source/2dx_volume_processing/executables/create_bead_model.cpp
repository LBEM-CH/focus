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
    std::string prog_help = "A program to generate bead model of the input map/mrc.";
    
    //Parse the arguments
    std::vector<args::Argument> program_args = 
        {args::Argument::mrcin, args::Argument::max_resolution, args::Argument::number_of_beads, args::Argument::density_threshold,
         args::Argument::hklout, args::Argument::mrcout};
    args::ArgumentParser parser(program_args, argc, argv, prog_help);
    
    //Get and check the variables
    std::cout << "::Reading arguments from command line:\n";
    std::string mrcin = parser.get(args::Argument::mrcin, true);
    double max_resolution = parser.get_double(args::Argument::max_resolution, true);
    int no_of_beads = parser.get_int(args::Argument::number_of_beads, true);
    double density_threshold = parser.get_double(args::Argument::density_threshold, true);
    
    std::string hklout = parser.get(args::Argument::hklout);
    std::string mrcout = parser.get(args::Argument::mrcout);
    
    if(hklout == "" && mrcout == "")
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout!\n";
        std::cerr << parser;
        exit(1);
    }
    
    //Prepare the input
    Volume2dx input;
    input.read_volume(mrcin);
    
    Volume2dx bead_model = input.generate_bead_model(no_of_beads, density_threshold, max_resolution);
    
    if(hklout != "") bead_model.write_volume(hklout, "hkl");
    if(mrcout != "") bead_model.write_volume(mrcout);
    
    return 0;
    
}
