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
    
    if(argc < 9)
    {
        std::cout << "Program Options\n\t<hkl file> <map file> <nx> <ny> <nz> <gamma> <MaxResolution> <Symmetry>\n";
        return(1);
    }
    
    std::cout <<"Starting the program..\n";
    
    std::string hklFileName = argv[1];
    std::string mrcFileName = argv[2];
    
    int nx = std::atoi(argv[3]);
    int ny = std::atoi(argv[4]);
    int nz = std::atoi(argv[5]);
    
    double gamma = std::atof(argv[6]);
    double max_resolution = std::atof(argv[7]);
    std::string symmetry = argv[8];

    std::cout << "Reading of arguments done.. \n";
    
    Volume2dx volume(nx, ny, nz);
    volume.read_volume(hklFileName, "hkl");
    volume.set_gamma(volume::utilities::angle_utilities::DegreeToRadian(gamma));
    volume.set_max_resolution(max_resolution);
    volume.set_symmetry(symmetry);
    volume.rescale_to_max_amplitude(20000);
    volume.symmetrize();
    volume.invert_hand();
    
    volume.write_volume(mrcFileName, "map");
    
}
