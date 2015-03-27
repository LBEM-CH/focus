/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#include <iostream>
#include "../src/data_structures/volume_header.hpp"
#include "../src/data_structures/fourier_space_data.hpp"
#include "../src/symmetization/symmetry2dx.hpp"
#include "../src/symmetization/fourier_symmetrization.hpp"
#include "../src/io/hkz_reader.hpp"
#include "../src/io/hkl_writer.hpp"
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
    
    std::string hkzFileName = argv[1];
    
    std::string symmetry = argv[2];
    
    int nx = std::atoi(argv[3]);
    int ny = std::atoi(argv[4]);
    int nz = std::atoi(argv[5]);
    
    double gamma = std::atof(argv[6]);
    double max_resolution = std::atof(argv[7]);
      
    //Prepare the header
    volume_processing_2dx::data_structures::VolumeHeader2dx header(nx, ny, nz);
    header.set_gamma(volume_processing_2dx::utilities::angle_utilities::DegreeToRadian(gamma));
    header.set_max_resolution(max_resolution);
    header.set_symmetry(symmetry);
    
    //Read in data
    volume_processing_2dx::data_structures::FourierSpaceData fourier_data = 
        volume_processing_2dx::io::hkz_reader::read(hkzFileName, header);
    
    //Symmetrize
    volume_processing_2dx::symmetrization::Symmetry2dx sym(symmetry);
    volume_processing_2dx::symmetrization::fourier_symmetrization::symmetrize(fourier_data, sym);
    
    //Write out data
    volume_processing_2dx::io::hkl_writer::write("output.hkl", fourier_data);
    
}

