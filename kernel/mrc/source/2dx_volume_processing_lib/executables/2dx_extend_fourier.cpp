/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#include <iostream>
#include <string.h>

#include "../include/2dx_volume_processing.h"

/**
 * A function to symmetrize HKL data and convert it to equally spaced fourier
 * data. In the process also symmetrize the volume in Fourier space to full P1 plane.
 * @param argc
 * @param argv
 * @return 
 */
int main(int argc, char** argv)
{
    //Read the program inputs:
    //<prog_name> hkz_file symmetry nx ny nz gamma max_resolution
    
    if (argc < 7) {
        std::cout << "Program Options\n\t<hklfile> <symmetry> <nx> <ny> <nz> <outfile>\n";
        std::cin.get();
    }
    
    std::cout << "Starting the program\n";
    
    std::string hklFileName = argv[1];
    
    std::string symmetry = argv[2];
    
    int nx = std::atoi(argv[3]);
    int ny = std::atoi(argv[4]);
    int nz = std::atoi(argv[5]);
    
    std::string outputFileName = argv[6];
    
    std::cout << "Reading of arguments done.. \n";
    
    Volume2dx volume(nx, ny, nz);
    volume.read_volume(hklFileName, "hkl");
    volume.rescale_to_max_amplitude(20000);
    
    volume.set_symmetry(symmetry);
    
    //Symmetrize
    volume.symmetrize();
    
    //Write out data
    volume.extend_to_full_fourier();
    volume.write_volume(outputFileName, "hkl");
    
}

