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
    
    if(argc < 6)
    {
        std::cout << "Program Options\n\t<mrc file> <pdb file> <no_of_beads> <density_threshold> <noise_level>\n";
        return(1);
    }
    
    std::cout <<"Starting the program..\n";
    
    std::string mrcFileName = argv[1];
    std::string pdbFile = argv[2];
    
    int no_of_beads = std::atoi(argv[3]);
    double density_threshold = std::atof(argv[4]);
    double noise_level = std::atof(argv[5]);
    
    Volume2dx volume;
    volume.read_volume(mrcFileName, "mrc");
    volume.write_bead_model_pdb(no_of_beads, density_threshold, noise_level, pdbFile);
    
    
}
