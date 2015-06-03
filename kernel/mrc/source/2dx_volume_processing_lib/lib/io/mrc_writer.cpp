/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <fstream>
#include <string.h>

#include "mrc_writer.hpp"

#include "../utilities/filesystem.hpp"
#include "../utilities/angle_utilities.hpp"

namespace ds = volume::data;

void volume::io::mrc_writer::write_real
        (const std::string file_name, 
        const volume::data::VolumeHeader2dx& header, 
        const volume::data::RealSpaceData& data)
{
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
    int nxstart = header.nxstart();
    int nystart = header.nystart();
    int nzstart = header.nzstart(); 
    float xlen = (float) header.xlen();
    float ylen = (float) header.ylen();
    float zlen = (float) header.zlen();
    float gamma = (float) volume::utilities::angle_utilities::RadianToDegree(header.gamma());
    float amin = (float) data.min();
    float amax = (float) data.max();
    float amean = (float) data.mean();
    int spcgrp = 1;

    int mode = 2;
    int mapc = 1;
    int mapr = 2;
    int maps = 3;
    float ninty = 90.0;
    int zero = 0;
    file.write((char*)&nx, sizeof(int));
    file.write((char*)&ny, sizeof(int));
    file.write((char*)&nz, sizeof(int));
    file.write((char*)&mode, sizeof(int));
    file.write((char*)&nxstart, sizeof(int));   //nxstart
    file.write((char*)&nystart, sizeof(int));   //nystart
    file.write((char*)&nzstart, sizeof(int));   //nzstart
    file.write((char*)&nx, sizeof(int));        //mx
    file.write((char*)&ny, sizeof(int));        //my
    file.write((char*)&nz, sizeof(int));        //mz
    file.write((char*)&xlen, sizeof(float));
    file.write((char*)&ylen, sizeof(float));
    file.write((char*)&zlen, sizeof(float));
    file.write((char*)&ninty, sizeof(float));
    file.write((char*)&ninty, sizeof(float));
    file.write((char*)&gamma, sizeof(float));
    file.write((char*)&mapc, sizeof(int));
    file.write((char*)&mapr, sizeof(int));
    file.write((char*)&maps, sizeof(int));
    file.write((char*)&amin, sizeof(float));
    file.write((char*)&amax, sizeof(float));
    file.write((char*)&amean, sizeof(float));
    file.write((char*)&spcgrp, sizeof(int));
    
    //Fill the rest of data with zeros
    for(int i=0; i<231; i++) file.write((char*)&zero, sizeof(float));
    
    //Write the data
    file.seekp(1024);
    for(int id=0; id < data.size(); id++ )
    {
        long write_id = data.size() - id - 1;
        float value = (float)data.get_value_at(write_id);
        file.write((char*)&value, sizeof(float));
    }
    
    file.close();
}
