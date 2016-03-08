#include "file.hpp"

using namespace tdx;

File::File(const std::string& file_name, openmode mode) 
    : std::fstream(file_name.c_str(), mode)
{
    _filename = file_name;
}

std::string File::file_name() const
{
    return _filename;
}

bool File::exists() const
{
    struct stat buffer;   
    bool result = (stat (file_name().c_str(), &buffer) == 0);
    return result;
}

std::string File::extension() const
{
    std::string extension = "";
    
    if(file_name().find('.') != std::string::npos){
        extension = file_name().substr(file_name().find_last_of(".") + 1);
    }
    
    return extension; 
}

size_t File::file_size() const
{
    struct stat stat_buf;
    int rc = stat(file_name().c_str(), &stat_buf);
    return rc == 0 ? stat_buf.st_size : -1;  
}

std::string File::read_line()
{
    std::string line;
    std::getline(*this, line);
    return line;
}