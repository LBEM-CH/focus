/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <sys/stat.h>

#include "binary_file_utilities.hpp"

long tdx::utilities::binary_file_utilities::file_size(const std::string& filename)
{
    struct stat stat_buf;
    int rc = stat(filename.c_str(), &stat_buf);
    return rc == 0 ? stat_buf.st_size : -1;
}


int tdx::utilities::binary_file_utilities::read_int(std::istream& in_file)
{
    int value;
    in_file.read((char*)&value, sizeof(int));
    
    return value;
}

float tdx::utilities::binary_file_utilities::read_float(std::istream& in_file)
{
    float value;
    in_file.read((char*)&value, sizeof(float));
    
    return value;
}
