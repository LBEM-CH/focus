/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "hkz_reader.hpp"
#include "../utilities/filesystem.hpp"
#include "../utilities/angle_utilities.hpp"
#include "../utilities/fourier_utilities.hpp"

volume_processing_2dx::data_structures::FourierSpaceData volume_processing_2dx::io::hkz_reader::read(std::string file_path, 
        const volume_processing_2dx::data_structures::VolumeHeader2dx& header)
{
    namespace ds = volume_processing_2dx::data_structures;
    
    //Check for the presence of file
    if (!volume_processing_2dx::utilities::filesystem::FileExists(file_path)){
        std::cerr << "File not found: " << file_path << std::endl;
    }
    
    std::cout << "Reading HKZ file: " << file_path << std::endl;
    
    //Multi-map to put all the possible diffraction spots
    ds::DiffractionSpotMultiMap spot_multimap;

    //Temp int read variables
    int h_in, k_in, l_in, iq_in;
    double z_in, amplitude_in, phase_in, sig_amplitude_in, sig_phase_in;

    std::ifstream hkzFile(file_path);

    //Start reading in the file
    try
    {
        while (hkzFile >> h_in >> k_in >> z_in >> amplitude_in >> phase_in >> sig_amplitude_in >> sig_phase_in >> iq_in)
        {
            l_in = round(z_in * header.nz());
            
            //std::cout << "Reading line: " << h_in << " " << k_in << " " << l_in << "(" << z_in << ")" << " " << amplitude_in << " " << phase_in << "\n"; 
            
            ds::MillerIndex index_in(h_in, k_in, l_in);
            double resolution =  volume_processing_2dx::utilities::fourier_utilities::GetResolution(
                                        index_in, header.gamma(), header.xlen(), header.ylen(), header.zlen());

            //Convert phase to radians
            phase_in = volume_processing_2dx::utilities::angle_utilities::DegreeToRadian(phase_in);

            //Covert also the sphaseIn to radians
            if(sig_phase_in>90) sig_phase_in = 0;
            sig_phase_in = volume_processing_2dx::utilities::angle_utilities::DegreeToRadian(sig_phase_in);

            if(resolution > header.max_resolution())
            {
                double weight_in = cos(sig_phase_in);
                double real_in = weight_in*amplitude_in*cos(phase_in);
                double imag_in = weight_in*amplitude_in*sin(phase_in);

                ds::Complex2dx complex_in(real_in, imag_in);
                ds::DiffractionSpot value_in(complex_in, weight_in);
                spot_multimap.insert(ds::MillerIndexDiffSpotPair(index_in, value_in));
            }

        }
    }
    catch(const std::exception& e)
    {
        std::cerr << "Error in reading HKZ file " << file_path << std::endl;
    }
    
    hkzFile.close();

    //Assign the data to fourier_space
    ds::FourierSpaceData fourier_data(spot_multimap);
    
    std::cout << "Data read complete.\n";
    
    return fourier_data;
}
