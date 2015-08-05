/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <sys/stat.h>
#include <sstream>
#include <fstream>

#include "filesystem.hpp"

bool volume::utilities::filesystem::FileExists(const std::string file_path)
{
    struct stat buffer;   
    bool result = (stat (file_path.c_str(), &buffer) == 0);
    return result;
}

std::string volume::utilities::filesystem::FileExtension(const std::string file_path)
{
    std::string extension = "";
    
    if(file_path.find('.') != std::string::npos){
        extension = file_path.substr(file_path.find_last_of(".") + 1);
    }
    
    return extension; 
}

int volume::utilities::filesystem::NumberOfColumns(const std::string file_name)
{
    std::ifstream infile(file_name);

    std::string sLine;
    int n=0;
    
    if (infile.good())
    {
        std::getline(infile, sLine);
        std::stringstream is(sLine);
        
        float temp;
        while (is >> temp)
        {
            n++;
        }
    }
    
    infile.close();
    
    return n;
}
