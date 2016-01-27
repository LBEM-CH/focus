/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "mrc_io.hpp"

#include "../utilities/binary_file_utilities.hpp"
#include "../utilities/filesystem.hpp"
#include "../utilities/angle_utilities.hpp"

namespace io = tdx::io;

tdx::data::VolumeHeader io::mrc::get_header(const std::string file_name, const std::string format)
{
    std::cout << "Reading header..\n";
    //Check for the presence of file
    if (!tdx::utilities::filesystem::FileExists(file_name)){
        std::cerr << "File not found: " << file_name << std::endl;
        exit(1);
    }
    
    //Check the format
    if(!(format == "mrc" || format == "map"))
    {
        std::cerr << "ERROR while reading " << format << " file:\n"
                  << "\t" << file_name << "\n"
                  << "File format not supported. Only supported formats MRC/MAP\n";
        exit(1);
    }
    
    tdx::data::VolumeHeader header;
    
    std::ifstream file(file_name, std::ios::in|std::ios::binary);;
    
    file.seekg (0, std::ios::beg);
    try
    {
        namespace bf = tdx::utilities::binary_file_utilities;
        
        //Read the header
        header.set_rows(bf::read_int(file));
        header.set_columns(bf::read_int(file));
        header.set_sections(bf::read_int(file));
        
        int mode = bf::read_int(file);
        
        if(mode != 2)
        {
            std::cerr << "ERROR while reading " << format << " file:\n"
                      << "\t" << file_name << "\n"
                      << "The data format (MRC mode:" << mode << ") not supported!\n"
                      << "HINT:\n Only MRC mode - 2 is supported.\n"
                      << "If you don't have mode 2 please open the image in Chimera and store it as MRC.\n";
            exit(1);
        }
        
        header.set_nxstart(bf::read_int(file));
        header.set_nystart(bf::read_int(file));
        header.set_nzstart(bf::read_int(file));
        
        header.set_mx(bf::read_int(file));
        header.set_my(bf::read_int(file));
        header.set_mz(bf::read_int(file));
        
        header.set_xlen(bf::read_float(file));
        header.set_ylen(bf::read_float(file));
        header.set_zlen(bf::read_float(file));
        
        float alpha = bf::read_float(file);
        float beta = bf::read_float(file);
        
        if(alpha != 90.0 || beta != 90.0)
        {
            std::cerr << "ERROR while reading " << format << " file:\n"
                      << "\t" << file_name << "\n"
                      << "The cell angles (" << alpha << "," << beta << ", ...) are not possible in 2D crystallography!\n";
            exit(1);
        }
        
        header.set_gamma(tdx::utilities::angle_utilities::DegreeToRadian(bf::read_float(file)));
        
        int mapc = bf::read_int(file);
        int mapr = bf::read_int(file);
        int maps = bf::read_int(file);
        
        if(mapc != 1 || mapr != 2 || maps !=3)
        {
            std::cerr << "ERROR while reading " << format << " file:\n"
                      << "\t" << file_name << "\n"
                      << "The axis for columns, rows and sections should be 1, 2, 3.\n"
                      << "Found:" << mapc << ", " << mapr << ", " << maps <<"\n";
            exit(1);
        }
        
    }
    catch(const std::exception& e)
    {
        std::cerr << "Error in reading " << format << " file header from:\n\t" << file_name << std::endl;
        exit(1);
    }
    
    file.close();
    return header;
}

tdx::data::RealSpaceData io::mrc::get_data(const std::string file_name, const int nx, const int ny, const int nz)
{
    std::cout << "Reading data..\n";
    //Check for the presence of file
    if (!tdx::utilities::filesystem::FileExists(file_name)){
        std::cerr << "File not found: " << file_name << std::endl;
        exit(1);
    }
    
    std::ifstream file(file_name, std::ios::in|std::ios::binary);
    
    long input_size = nx*ny*nz;
    long file_size = tdx::utilities::binary_file_utilities::file_size(file_name);
    long memory_size = (int)(file_size - 1024)/4;
    
    if(memory_size < input_size)
    {
        std::cerr << "ERROR while reading file:\n"
                  << "\t" << file_name << "\n"
                  << "Error reading data, input data is less than expected.\n"
                  << "\tExpected size: (" << nx << "X" << ny << "X" << nz << ") " << input_size <<"\n"
                  << "\tData size in file: " << memory_size << "\n";       
        exit(1);
    }
    
    file.seekg (1024, std::ios::beg);
    
    float* _data = (float*) malloc(input_size*sizeof(float));
    for(int itr_memory=0; itr_memory < input_size; ++itr_memory)
    {
        _data[itr_memory] = tdx::utilities::binary_file_utilities::read_float(file);
    }
    
    tdx::data::RealSpaceData data(nx, ny, nz);
    for(int id=0; id< data.size(); ++id)
    {
        //flip the data for 2dx format
        data.set_value_at(data.size()-id-1, (double)_data[id]);     
    }
    
    free(_data);
    return data;
    
}

void tdx::io::mrc::write_mrc_mode_2(const std::string file_name, 
                                            const tdx::data::VolumeHeader& header, 
                                            const tdx::data::RealSpaceData& data,
                                            const std::string format)
{
    //Check for the existence of the file
    if(tdx::utilities::filesystem::FileExists(file_name))
    {
        std::cout << "WARNING: File.. " << file_name << " already exists. Overwriting!\n";
    }
    
    //Create output stream
    std::ofstream file(file_name, std::ios::out|std::ios::binary);
    
    //Write the header
    int rows = header.rows();
    int columns = header.columns();
    int sections = header.sections();
    int nxstart = header.nxstart();
    int nystart = header.nystart();
    int nzstart = header.nzstart();
    int mx = header.mx();
    int my = header.my();
    int mz = header.mz();
    float xlen = (float) header.xlen();
    float ylen = (float) header.ylen();
    float zlen = (float) header.zlen();
    float gamma = (float) tdx::utilities::angle_utilities::RadianToDegree(header.gamma());
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
    
    float ccp4_skwmat = 0.0101;
    int machine_stamp = 16708;
    
    file.write((char*)&rows, sizeof(int));
    file.write((char*)&columns, sizeof(int));
    file.write((char*)&sections, sizeof(int));
    file.write((char*)&mode, sizeof(int));
    file.write((char*)&nxstart, sizeof(int));   //nxstart
    file.write((char*)&nystart, sizeof(int));   //nystart
    file.write((char*)&nzstart, sizeof(int));   //nzstart
    file.write((char*)&mx, sizeof(int));        //mx
    file.write((char*)&my, sizeof(int));        //my
    file.write((char*)&mz, sizeof(int));        //mz
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
      
    if(format == "mrc")
    {
        //Fill the rest of data with zeros
        for(int i=0; i<233; i++) file.write((char*)&zero, sizeof(float));
    }
    else if(format == "map")
    {
        //Fill few default fields expected by CCP4
        file.write((char*)&zero, sizeof(int));
        file.write((char*)&zero, sizeof(int));
        file.write((char*)&ccp4_skwmat, sizeof(float));
        file.write((char*)&zero, sizeof(float));
        file.write((char*)&zero, sizeof(float));
        file.write((char*)&zero, sizeof(float));
        file.write((char*)&ccp4_skwmat, sizeof(float));
        file.write((char*)&zero, sizeof(float));
        file.write((char*)&zero, sizeof(float));
        file.write((char*)&zero, sizeof(float));
        file.write((char*)&ccp4_skwmat, sizeof(float));

        for(int i=0; i<18; i++) file.write((char*)&zero, sizeof(float));

        file.write("MAP ", sizeof(int));
        file.write((char*)&machine_stamp, sizeof(int));
        file.write((char*)&zero, sizeof(int));
        file.write((char*)&zero, sizeof(int));
    
        //Fill the rest of data with zeros
        for(int i=0; i<200; i++) file.write("    ", sizeof(float));
    }
    
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
