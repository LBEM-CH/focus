/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "hkl_writer.hpp"



void volume::io::hkl_writer::write(const std::string& file_path, const volume::data::FourierSpaceData& data)
{
    const int INT_WIDTH = 5;
    const int FLOAT_WIDTH = 13;
    const int FLOAT_PRECISION = 7;

    std::ofstream hklFile(file_path);

    std::cout << "Writing the hkl file.. " << file_path << std::endl;

    for(volume::data::FourierSpaceData::const_iterator ii=data.begin(); ii!=data.end(); ++ii){
        int h = (*ii).first.h();
        int k = (*ii).first.k();
        int l = (*ii).first.l();
        double amp = (*ii).second.value().amplitude();
        double phase = ((*ii).second.value().phase())*180/M_PI;
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
