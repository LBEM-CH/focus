/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include "filesystem.hpp"

bool volume_processing_2dx::utilities::filesystem::FileExists(const std::string file_path)
{
    struct stat buffer;   
    bool result = (stat (file_path.c_str(), &buffer) == 0);
    return result;
}

std::string volume_processing_2dx::utilities::filesystem::FileExtension(const std::string file_path)
{
    std::string extension = "";
    
    if(file_path.find('.') != std::string::npos){
        extension = file_path.substr(file_path.find_last_of(".") + 1);
    }
    
    return extension; 
}
