/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <fstream>

#include "mrc_writer.hpp"

#include "../utilities/filesystem.hpp"
#include "../utilities/angle_utilities.hpp"

namespace ds = volume::data;

void volume::io::mrc_writer::write_real
        (const std::string file_name, 
        const volume::data::VolumeHeader2dx& header, 
        const volume::data::RealSpaceData& data)
{
    std::cout << "Writing MRC file to " << file_name << std::endl;
    
    //Check for the existence of the file
    if(volume::utilities::filesystem::FileExists(file_name))
    {
        std::cout << "WARNING: File.. " << file_name << " already exists. Overwriting!\n";
    }
    
    //Create output stream
    std::ofstream file(file_name, std::ios::out|std::ios::binary);
    
    //Write the header
    int nx = header.nx();
    int ny = header.ny();
    int nz = header.nz();
    float xlen = (float) header.xlen();
    float ylen = (float) header.ylen();
    float zlen = (float) header.zlen();
    float gamma = (float) volume::utilities::angle_utilities::RadianToDegree(header.gamma());
    int zero = 0;
    int mode = 2;
    float ninty = 90.0;
    file.write((char*)&nx, sizeof(int));
    file.write((char*)&ny, sizeof(int));
    file.write((char*)&nz, sizeof(int));
    file.write((char*)&mode, sizeof(int));
    file.write((char*)&zero, sizeof(int));  //nxstart
    file.write((char*)&zero, sizeof(int));  //nystart
    file.write((char*)&zero, sizeof(int));  //nzstart
    file.write((char*)&nx, sizeof(int));    //mx
    file.write((char*)&ny, sizeof(int));    //my
    file.write((char*)&nz, sizeof(int));    //mz
    file.write((char*)&xlen, sizeof(float));
    file.write((char*)&ylen, sizeof(float));
    file.write((char*)&zlen, sizeof(float));
    file.write((char*)&ninty, sizeof(float));
    file.write((char*)&ninty, sizeof(float));
    file.write((char*)&gamma, sizeof(float));
    for(int i=0; i<240; i++) file.write((char*)&zero, sizeof(float));
    
    //Write the data
    file.seekp(1024);
    for(int id=0; id < data.size(); id++ )
    {
        long write_id = data.size() - id;
        float value = (float)data.get_value_at(write_id);
        file.write((char*)&value, sizeof(float));
    }
    
    file.close();
    
    std::cout << "File written.\n";
}
