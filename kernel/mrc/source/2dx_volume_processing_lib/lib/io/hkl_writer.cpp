/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "hkl_writer.hpp"

#include "../utilities/angle_utilities.hpp"
#include "../utilities/filesystem.hpp"

void volume::io::hkl_writer::write(const std::string& file_path, const volume::data::FourierSpaceData& data, bool for_ccp4)
{
    const int INT_WIDTH = 5;
    const int FLOAT_WIDTH = 13;
    const int FLOAT_PRECISION = 7;

    std::ofstream hklFile(file_path);
    
    //Check for the existence of the file
    if(volume::utilities::filesystem::FileExists(file_path))
    {
        std::cout << "WARNING: File.. " << file_path << " already exists. Overwriting!\n";
    }

    volume::data::FourierSpaceData data_to_write = data;
    
    //If is being generated for CCP4 invert the handedness
    if(for_ccp4) data_to_write = data.invert_hand();
    
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
