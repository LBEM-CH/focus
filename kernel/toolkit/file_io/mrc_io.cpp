/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <ctime>
#include "mrc_io.hpp"

#include "../basics/binary_file.hpp"
#include "../utilities/angle_utilities.hpp"

namespace io = tdx::io;

tdx::data::VolumeHeader io::mrc::get_header(const std::string file_name, const std::string format)
{
    std::cout << "Reading header..\n";
    
    tdx::BinaryFile infile(file_name, tdx::File::in);
    //Check for the presence of file
    if (!infile.exists()){
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
    header.set_file_name(file_name);
    header.set_title("Read from file with format: " + format);
    
    infile.seekg (0, std::ios::beg);
    try
    {   
        //Read the header
        header.set_rows(infile.read_int());
        header.set_columns(infile.read_int());
        header.set_sections(infile.read_int());
        
        int mode = infile.read_int();
        
        if(mode != 2)
        {
            std::cerr << "ERROR while reading " << format << " file:\n"
                      << "\t" << file_name << "\n"
                      << "The data format (MRC mode:" << mode << ") not supported!\n"
                      << "HINT:\n Only MRC mode - 2 is supported.\n"
                      << "If you don't have mode 2 please open the image in Chimera and store it as MRC.\n";
            exit(1);
        }
        
        header.set_nxstart(infile.read_int());
        header.set_nystart(infile.read_int());
        header.set_nzstart(infile.read_int());
        
        header.set_mx(infile.read_int());
        header.set_my(infile.read_int());
        header.set_mz(infile.read_int());
        
        header.set_xlen(infile.read_float());
        header.set_ylen(infile.read_float());
        header.set_zlen(infile.read_float());
        
        float alpha = infile.read_float();
        float beta = infile.read_float();
        
        if(alpha != 90.0 || beta != 90.0)
        {
            std::cerr << "ERROR while reading " << format << " file:\n"
                      << "\t" << file_name << "\n"
                      << "The cell angles (" << alpha << "," << beta << ", ...) are not possible in 2D crystallography!\n";
            exit(1);
        }
        
        header.set_gamma(tdx::utilities::angle_utilities::DegreeToRadian(infile.read_float()));
        
        int mapc = infile.read_int();
        int mapr = infile.read_int();
        int maps = infile.read_int();
        
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
    
    infile.close();
    return header;
}

tdx::data::RealSpaceData io::mrc::get_data(const std::string file_name, const int nx, const int ny, const int nz)
{
    std::cout << "Reading data..\n";
    
    tdx::BinaryFile infile(file_name, tdx::File::in);
    
    //Check for the presence of file
    if (!infile.exists()){
        std::cerr << "File not found: " << file_name << std::endl;
        exit(1);
    }
    
    size_t input_size = nx*ny*nz;
    size_t file_size = infile.file_size();
    size_t memory_size = (int)(file_size - 1024)/4;
    
    if(memory_size < input_size)
    {
        std::cerr << "ERROR while reading file:\n"
                  << "\t" << file_name << "\n"
                  << "Error reading data, input data is less than expected.\n"
                  << "\tExpected size: (" << nx << "X" << ny << "X" << nz << ") " << input_size <<"\n"
                  << "\tData size in file: " << memory_size << "\n";       
        exit(1);
    }
    
    infile.seekg (1024, std::ios::beg);
    
    clock_t start = clock();
    float* _data = (float*) malloc(input_size*sizeof(float));
    for(int itr_memory=0; itr_memory < input_size; ++itr_memory)
    {
        _data[itr_memory] = infile.read_float();
    }
    
    std::cout << "Data read in " << (clock() - start)/(double)CLOCKS_PER_SEC << " seconds\n";

    start = clock();
    tdx::data::RealSpaceData data(nx, ny, nz);
    for(int id=0; id< data.size(); ++id)
    {
        //flip the data for 2dx format
        data.set_value_at(data.size()-id-1, (double)_data[id]);     
    }
    
    std::cout << "Data flipped in " << (clock() - start)/(double)CLOCKS_PER_SEC << " seconds\n";
    
    free(_data);
    return data;
    
}

void tdx::io::mrc::write_mrc_mode_2(const std::string file_name, 
                                            const tdx::data::VolumeHeader& header, 
                                            const tdx::data::RealSpaceData& data,
                                            const std::string format)
{
    
    tdx::File outfile(file_name, tdx::File::out);
    
    //Check for the existence of the file
    if(outfile.exists())
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
    
    clock_t start = clock();
    
    outfile.write((char*)&rows, sizeof(int));
    outfile.write((char*)&columns, sizeof(int));
    outfile.write((char*)&sections, sizeof(int));
    outfile.write((char*)&mode, sizeof(int));
    outfile.write((char*)&nxstart, sizeof(int));   //nxstart
    outfile.write((char*)&nystart, sizeof(int));   //nystart
    outfile.write((char*)&nzstart, sizeof(int));   //nzstart
    outfile.write((char*)&mx, sizeof(int));        //mx
    outfile.write((char*)&my, sizeof(int));        //my
    outfile.write((char*)&mz, sizeof(int));        //mz
    outfile.write((char*)&xlen, sizeof(float));
    outfile.write((char*)&ylen, sizeof(float));
    outfile.write((char*)&zlen, sizeof(float));
    outfile.write((char*)&ninty, sizeof(float));
    outfile.write((char*)&ninty, sizeof(float));
    outfile.write((char*)&gamma, sizeof(float));
    outfile.write((char*)&mapc, sizeof(int));
    outfile.write((char*)&mapr, sizeof(int));
    outfile.write((char*)&maps, sizeof(int));
    outfile.write((char*)&amin, sizeof(float));
    outfile.write((char*)&amax, sizeof(float));
    outfile.write((char*)&amean, sizeof(float));
    outfile.write((char*)&spcgrp, sizeof(int));
      
    /* CHEN>
    if(format == "mrc")
    {
        //Fill the rest of data with zeros
        for(int i=0; i<233; i++) file.write((char*)&zero, sizeof(float));
    }
    else if(format == "map")
    {
    */
    
        //Fill few default fields expected by CCP4
        outfile.write((char*)&zero, sizeof(int));
        outfile.write((char*)&zero, sizeof(int));
        outfile.write((char*)&ccp4_skwmat, sizeof(float));
        outfile.write((char*)&zero, sizeof(float));
        outfile.write((char*)&zero, sizeof(float));
        outfile.write((char*)&zero, sizeof(float));
        outfile.write((char*)&ccp4_skwmat, sizeof(float));
        outfile.write((char*)&zero, sizeof(float));
        outfile.write((char*)&zero, sizeof(float));
        outfile.write((char*)&zero, sizeof(float));
        outfile.write((char*)&ccp4_skwmat, sizeof(float));

        for(int i=0; i<18; i++) outfile.write((char*)&zero, sizeof(float));

        outfile.write("MAP ", sizeof(int));
        outfile.write((char*)&machine_stamp, sizeof(int));
        outfile.write((char*)&zero, sizeof(int));
        outfile.write((char*)&zero, sizeof(int));
    
        //Fill the rest of data with zeros
        for(int i=0; i<200; i++) outfile.write("    ", sizeof(float));
    /* CHEN>
    }
    */
    
    //Write the data
    outfile.seekp(1024);
    for(int id=0; id < data.size(); id++ )
    {
        long write_id = data.size() - id - 1;
        float value = (float)data.get_value_at(write_id);
        outfile.write((char*)&value, sizeof(float));
    }
    
    std::cout << "File written in " << (clock()-start)/(double)CLOCKS_PER_SEC << " seconds\n";
    
    outfile.close();
}
