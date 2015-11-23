/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iomanip>

#include "reflection_io.hpp"
#include "../utilities/filesystem.hpp"
#include "../utilities/angle_utilities.hpp"
#include "../utilities/fourier_utilities.hpp"

volume::data::FourierSpaceData volume::io::reflection::read(std::string file_path, int z_scale, bool raw_ccp4)
{
    namespace ds = volume::data;
    
    //Check for the presence of file
    if (!volume::utilities::filesystem::FileExists(file_path)){
        std::cerr << "File not found: " << file_path << std::endl;
        exit(1);
    }
    
    //Multi-map to put all the possible diffraction spots
    ds::DiffractionSpotMultiMap spot_multimap;
    
    //Temp read variables
    int h_in, k_in;
    double z_in, amplitude_in, phase_in, wt_in, dummy;
    
    //Count number of columns in file
    int number_columns;
    int header_lines = volume::utilities::filesystem::NumberOfColumns(file_path, number_columns);
    
    if(number_columns < 5 )
    {
        std::cerr << "ERROR while reading reflections file:\n"
                  << "\t" << file_path << "\n"
                  << "Minimum expected number of columns is 5, found: " << number_columns << "\n";
        exit(1);
    }
    
    std::ifstream hkzFile(file_path);
    
    if(header_lines != 0)
    {
        std::cout << "WARNING: Found " << header_lines << " header lines while reading reflections file " << file_path << "\n\n";
    }
    for(int header_line=0; header_line<header_lines; header_line++)
    {
        std::string sLine;
        std::getline(hkzFile, sLine);
    }
    
    if(number_columns == 5)
    {
        std::cout << "\nExpected column order from reflections file:\n";
        std::cout << "----------------------------------------------\n";
        std::cout << "H K L/Z* AMP PHASE\n";
        std::cout << "----------------------------------------------\n\n";
        while (hkzFile >> h_in >> k_in >> z_in >> amplitude_in >> phase_in)
        {
            add_spot(spot_multimap, h_in, k_in, z_in, amplitude_in, phase_in, 1.0, z_scale, raw_ccp4);
        }
    }
    
    if(number_columns == 6)
    {
        std::cout << "\nExpected column order from reflections file:\n";
        std::cout << "----------------------------------------------\n";
        std::cout << "H K L/Z* AMP PHASE FOM\n";
        std::cout << "----------------------------------------------\n\n";
        while (hkzFile >> h_in >> k_in >> z_in >> amplitude_in >> phase_in >> wt_in)
        {
            if(wt_in > 1.0) wt_in = wt_in*0.01;
            add_spot(spot_multimap, h_in, k_in, z_in, amplitude_in, phase_in, wt_in, z_scale, raw_ccp4);
        }
    }
    
    if(number_columns == 7)
    {
        std::cout << "\nExpected column order from reflections file:\n";
        std::cout << "----------------------------------------------\n";
        std::cout << "H K L/Z* AMP PHASE FOM DUMMY\n";
        std::cout << "----------------------------------------------\n\n";
        while (hkzFile >> h_in >> k_in >> z_in >> amplitude_in >> phase_in >> wt_in >> dummy)
        {
            if(wt_in > 1.0) wt_in = wt_in*0.01;
            add_spot(spot_multimap, h_in, k_in, z_in, amplitude_in, phase_in, wt_in, z_scale, raw_ccp4);
        }
    }
    
    if(number_columns == 8)
    {
        std::cout << "\nExpected column order from reflections file:\n";
        std::cout << "----------------------------------------------\n";
        std::cout << "H K L/Z* AMP PHASE DUMMY SIG_PHASE DUMMY\n";
        std::cout << "----------------------------------------------\n\n";
        while (hkzFile >> h_in >> k_in >> z_in >> amplitude_in >> phase_in >> dummy >> wt_in >> dummy)
        {
            if(wt_in>90) wt_in = 90;
            if ( wt_in < 89.9 )
            {   wt_in = volume::utilities::angle_utilities::DegreeToRadian(wt_in);
                add_spot(spot_multimap, h_in, k_in, z_in, amplitude_in, phase_in, cos(wt_in), z_scale, raw_ccp4);
            }
        }
    }
    
    if(number_columns > 8)
    {
        std::cerr << "ERROR while reading reflections file:\n"
                  << "\t" << file_path << "\n"
                  << "Maximum expected number of columns is 8, found: " << number_columns << "\n";
        exit(1);
    }
    
    hkzFile.close();

    //Assign the data to Fourier_space
    ds::FourierSpaceData fourier_data(spot_multimap);
    
    return fourier_data;
}

void volume::io::reflection::write(const std::string& file_path, const volume::data::FourierSpaceData& data, bool for_ccp4)
{
    const int INT_WIDTH = 5;
    const int FLOAT_WIDTH = 13;
    const int FLOAT_PRECISION = 7;

    //Check for the existence of the file
    if(volume::utilities::filesystem::FileExists(file_path))
    {
        std::cout << "WARNING: File.. " << file_path << " already exists. Overwriting!\n";
    }
    
    std::ofstream hklFile(file_path);
    
    volume::data::FourierSpaceData data_to_write = data;
    
    //If is being generated for CCP4 invert the handedness
    //if(for_ccp4) data_to_write = data.invert_hand();
    
    std::cout << "\nWriting following columns to reflections file:\n";
    std::cout << "----------------------------------------------\n";
    std::cout << "H K L AMP PHASE FOM\n";
    std::cout << "----------------------------------------------\n\n";
    
    for(volume::data::FourierSpaceData::const_iterator ii=data_to_write.begin(); ii!=data_to_write.end(); ++ii){
        int h = (*ii).first.h();
        int k = (*ii).first.k();
        int l = (*ii).first.l();
        
        double amp = (*ii).second.value().amplitude();
        double phase = ((*ii).second.value().phase());
        
        //If is being generate for CCP4 shift the phase to de-centerize the density
        if(for_ccp4) phase = phase + M_PI*l;
        
        phase = volume::utilities::angle_utilities::CorrectRadianPhase(phase);
        
        phase = volume::utilities::angle_utilities::RadianToDegree(phase);
        
        double fom = (*ii).second.weight()*100;

        hklFile << std::setw(INT_WIDTH) << h << " "
                << std::setw(INT_WIDTH) << k << " "
                << std::setw(INT_WIDTH) << l << " "
                << std::setw(FLOAT_WIDTH) << std::setprecision(FLOAT_PRECISION) << amp << " "
                << std::setw(FLOAT_WIDTH) << std::setprecision(FLOAT_PRECISION) << phase << " "
                << std::setw(FLOAT_WIDTH) << std::setprecision(FLOAT_PRECISION) << fom << std::endl;
    }

    hklFile.close();
}

void volume::io::reflection::add_spot(volume::data::DiffractionSpotMultiMap& map, int h_in, int k_in, double z_in, double amp_in, double phase_in, double weight_in, int z_scale, bool raw_ccp4)
{
    namespace ds= volume::data;
    
    int l_in = round(z_in * z_scale);
    ds::MillerIndex index_in(h_in, k_in, l_in);
    
    //Set the density to center if is CCP4 generated raw data 
    if(raw_ccp4) phase_in = phase_in + 180*(l_in);
    
    //Convert phase to radians
    phase_in = volume::utilities::angle_utilities::DegreeToRadian(phase_in);
    
    double real_in = amp_in*cos(phase_in);
    double imag_in = amp_in*sin(phase_in);

    ds::Complex2dx complex_in(real_in, imag_in);
    ds::DiffractionSpot value_in(complex_in, weight_in);
    
    map.insert(ds::MillerIndexDiffSpotPair(index_in, value_in));
}