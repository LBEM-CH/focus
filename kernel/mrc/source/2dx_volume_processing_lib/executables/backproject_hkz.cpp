/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#include <iostream>
#include <string.h>

#include "../src/data_structures/volume2dx.hpp"
#include "../src/utilites/angle_utilities.hpp"

/**
 * A function to backproject HKZ data and convert it to equally spaced fourier
 * data. In the process also symmetrize the volume in Fourier space.
 * @param argc
 * @param argv
 * @return 
 */
int main(int argc, char** argv)
{
    //Read the program inputs:
    //<prog_name> hkz_file symmetry nx ny nz gamma max_resolution
    
    if (argc < 7) {
        std::cout << "Program Options\n\t<hkzfile> <symmetry> <nx> <ny> <nz> <gamma> <max_resolution>\n";
        std::cin.get();
    }
    
    std::cout << "Starting the program\n";
    
    std::string hkzFileName = argv[1];
    
    std::string symmetry = argv[2];
    
    int nx = std::atoi(argv[3]);
    int ny = std::atoi(argv[4]);
    int nz = std::atoi(argv[5]);
    
    double gamma = std::atof(argv[6]);
    double max_resolution = std::atof(argv[7]);
    
    std::cout << "Reading of arguments done.. \n";
    
    volume_processing_2dx::data_structures::Volume2dx volume(nx, ny, nz);
    volume.read_volume(hkzFileName, "hkz");
    
    volume.set_gamma(volume_processing_2dx::utilities::angle_utilities::DegreeToRadian(gamma));
    volume.set_max_resolution(max_resolution);
    volume.set_symmetry(symmetry);
    
    //Symmetrize
    volume.symmetrize();
    
    //Write out data
    volume.write_volume("output.hkl");
    
}

