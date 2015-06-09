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
    
    std::string prog_help = "Program to produce the information of mrc/map.";
    
    //Parse the arguments
    std::vector<args::Argument> program_args = 
        {args::Argument::mrcin};
    args::ArgumentParser parser(program_args, argc, argv, prog_help);
    
    //Get and check the variables
    std::cout << "::Reading arguments from command line:\n";
    std::string mrcin = parser.get(args::Argument::mrcin, true);
    
    //Prepare the input
    Volume2dx input;
    input.read_volume(mrcin);
    std::cout << input.to_string();
    
    return 0;
}
