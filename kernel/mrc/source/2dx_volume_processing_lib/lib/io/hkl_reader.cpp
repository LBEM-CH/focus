/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "hkl_reader.hpp"
#include "../utilities/filesystem.hpp"
#include "../utilities/angle_utilities.hpp"
#include "../utilities/fourier_utilities.hpp"

volume::data::FourierSpaceData volume::io::hkl_reader::read(std::string file_path, 
        const volume::data::VolumeHeader2dx& header)
{
    namespace ds = volume::data;
    
    //Check for the presence of file
    if (!volume::utilities::filesystem::FileExists(file_path)){
        std::cerr << "File not found: " << file_path << std::endl;
    }
    
    //Multi-map to put all the possible diffraction spots
    ds::DiffractionSpotMultiMap spot_multimap;

    //Temp int read variables
    int h_in, k_in, l_in;
    double amplitude_in, phase_in, fom_in;

    std::ifstream hkzFile(file_path);

    //Start reading in the file
    try
    {
        while (hkzFile >> h_in >> k_in >> l_in >> amplitude_in >> phase_in >> fom_in)
        {
            
            ds::MillerIndex index_in(h_in, k_in, l_in);
           
            //Convert phase to radians
            phase_in = volume::utilities::angle_utilities::DegreeToRadian(phase_in);

            //Covert also the sphaseIn to radians
            
                double weight_in = fom_in/100;
                double real_in = amplitude_in*cos(phase_in);
                double imag_in = amplitude_in*sin(phase_in);

                ds::Complex2dx complex_in(real_in, imag_in);
                ds::DiffractionSpot value_in(complex_in, weight_in);
                spot_multimap.insert(ds::MillerIndexDiffSpotPair(index_in, value_in));
            

        }
    }
    catch(const std::exception& e)
    {
        std::cerr << "Error in reading HKL file " << file_path << std::endl;
    }
    
    hkzFile.close();

    //Assign the data to fourier_space
    ds::FourierSpaceData fourier_data(spot_multimap);
    
    return fourier_data;
}
