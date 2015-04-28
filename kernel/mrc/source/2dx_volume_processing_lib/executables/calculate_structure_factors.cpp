/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <string.h>
#include <fstream>
#include <iomanip> 
#include <map>

#include "../include/2dx_volume_processing.h"

int main(int argc, char** argv)
{
    
    if(argc < 3)
    {
        std::cout << "Program Options\n\t<mrc file> <output structure factor file>\n";
        return(1);
    }
    
    std::cout <<"Starting the program..\n";
    
    std::string mrcFileName = argv[1];
    std::string sf_file = argv[2];
    
    Volume2dx volume;
    volume.read_volume(mrcFileName, "mrc");
    
    volume::data::StructureFactors sf = volume.calculate_structure_factors(100);
    
    std::ofstream output(sf_file);
    output << sf.to_string();
    
    std::cout << sf.plot_profile();
    
}
