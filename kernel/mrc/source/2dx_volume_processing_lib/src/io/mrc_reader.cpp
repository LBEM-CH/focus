/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "mrc_reader.hpp"

#include "../utilites/binary_file_utilities.hpp"
#include "../utilites/angle_utilities.hpp"

namespace io = volume_processing_2dx::io;

io::MrcReader::MrcReader(std::string file_name)
{
    this->initialize(file_name);
}

bool io::MrcReader::initialize(std::string file_name)
{
    if(!(this->read_header(file_name)) || !(this->read_data(file_name)))
    {
        std::cerr << "Error in parsing MRC file\n";
        return false;
    }
    return true;
    
}

bool io::MrcReader::mrc_to_real(
            volume_processing_2dx::data_structures::VolumeHeader2dx& header, 
            volume_processing_2dx::data_structures::RealSpaceData& data)
{
    
    
    std::cout << "Processing MRC volume for 2dx..\n";
    
    if(mode != 2 || mx != nx || my != ny || mz != nz)
    {
        std::cerr << "MRC file in unsupported format!!\n";
        return false;
    }
    
    header.set_nx(nx);
    header.set_ny(ny);
    header.set_nz(nz);
    header.set_xlen((double)xlen);
    header.set_ylen((double)ylen);
    header.set_zlen((double)zlen);
    header.set_gamma(volume_processing_2dx::utilities::angle_utilities::DegreeToRadian(
                      (double)gamma));
    
    
    data.reset(nx, ny, nz);
    std::cout << "Flipping the data for 2dx!\n";
    for(int id=0; id< data.size(); ++id)
    {
        //flip the data
        data.set_value_at(data.size()-id-1, (double)this->_data[id]);     
    }
    
    std::cout << "Done assigning!\n";
    return true;
}

bool io::MrcReader::read_header(std::string file_name)
{
    std::cout << "Reading MRC file header from: " << file_name << std::endl;
    std::ifstream file(file_name, std::ios::in|std::ios::binary);;
    
    file.seekg (0, std::ios::beg);
    try
    {
        namespace bf = volume_processing_2dx::utilities::binary_file_utilities;
        //Read the header
        nx = bf::read_int(file);
        std::cout << "nx = " << nx << std::endl;
        ny = bf::read_int(file);
        std::cout << "ny = " << ny << std::endl;
        nz = bf::read_int(file);
        std::cout << "nz = " << nz << std::endl;
        
        mode = bf::read_int(file);
        std::cout << "mode = " << mode << std::endl;
        
        nxstart = bf::read_int(file);
        std::cout << "nxstart = " << nxstart << std::endl;
        nystart = bf::read_int(file);
        std::cout << "nystart = " << nystart << std::endl;
        nzstart = bf::read_int(file);
        std::cout << "nzstart = " << nzstart << std::endl;
        
        mx = bf::read_int(file);
        std::cout << "mx = " << mx << std::endl;
        my = bf::read_int(file);
        std::cout << "my = " << my << std::endl;
        mz = bf::read_int(file);
        std::cout << "mz = " << mz << std::endl;
        
        xlen = bf::read_float(file);
        std::cout << "xlen = " << xlen << std::endl;
        ylen = bf::read_float(file);
        std::cout << "ylen = " << ylen << std::endl;
        zlen = bf::read_float(file);
        std::cout << "zlen = " << zlen << std::endl;
        
        alpha = bf::read_float(file);
        std::cout << "alpha = " << alpha << " degrees" << std::endl;
        beta = bf::read_float(file);
        std::cout << "beta = " << beta << " degrees" << std::endl;
        gamma = bf::read_float(file);
        std::cout << "gamma = " << gamma << " degrees" << std::endl;
        
        mapc = bf::read_int(file);
        std::cout << "mapc = " << mapc << std::endl;
        mapr = bf::read_int(file);
        std::cout << "mapr = " << mapr << std::endl;
        maps = bf::read_int(file);
        std::cout << "maps = " << maps << std::endl;
        
        std::cout << "Header reading done.\n";
        
    }
    catch(const std::exception& e)
    {
        std::cerr << "Error in reading MRC file header" << file_name << std::endl;
        return false;
    }
    
    file.close();
    return true;
}

bool io::MrcReader::read_data(std::string file_name)
{
    std::cout << "Reading MRC file data from: " << file_name << std::endl;
    std::ifstream file(file_name, std::ios::in|std::ios::binary);
    
    long input_size = mx*my*mz;
    long file_size = volume_processing_2dx::utilities::binary_file_utilities::file_size(file_name);
    long memory_size = (int)(file_size - 1024)/4;
    
    std::cout << "Sizes (in number of floats):\n";
    std::cout << "Memory size: " << memory_size << " Input size: " << input_size << std::endl;
    
    if(memory_size < input_size)
    {
        std::cerr << "Error reading mrc data, input data is less than specified.";
        return false;
    }
    
    file.seekg (1024, std::ios::beg);
    
    _data = (float*) malloc(input_size*sizeof(float));
    for(int itr_memory=0; itr_memory < input_size; ++itr_memory)
    {
        _data[itr_memory] = volume_processing_2dx::utilities::binary_file_utilities::read_float(file);
    }
    
    std::cout << "Data reading done.\n";
            
    return true;
    
}