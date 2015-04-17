/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <string.h>

#include "../include/2dx_volume_processing.h"

int main(int argc, char** argv)
{
    
    if(argc < 4)
    {
        std::cout << "Required Program Options\n\t<mrc file> <reference mrc file> <output mrc file>\n";
        return(1);
    }
    
    std::cout <<"Starting the program..\n";
    
    std::string input_mrc_name = argv[1];
    std::string ref_mrc_name = argv[2];
    std::string output_mrc_name = argv[3];
    
    std::cout << "Setting INPUT VOLUME\n";
    Volume2dx input_volume;
    input_volume.read_volume(input_mrc_name, "mrc");

    std::cout << "Setting REFERENCE VOLUME\n";
    Volume2dx ref_volume;
    ref_volume.read_volume(ref_mrc_name, "mrc");
    
    std::cout << "Matching the histogram of INPUT volume to REFERENCE volume\n";
    input_volume.apply_density_histogram(ref_volume);
    
    std::cout << "Writing FINAL mrc file..\n";
    input_volume.write_volume(output_mrc_name, "mrc");
    
}
