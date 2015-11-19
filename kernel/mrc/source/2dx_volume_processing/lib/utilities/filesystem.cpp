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

int volume::utilities::filesystem::NumberOfColumns(const std::string file_name, int& number_columns)
{
    std::ifstream infile(file_name);

    std::string sLine;
    number_columns = 0;
    int cols_current = 0;
    int cols_previous=0;
    int header_lines=-1;
    
    if (infile.good())
    {
        //Check for the same number of columns in two consecutive rows
        while(! infile.eof())
        {
            std::getline(infile, sLine);
            std::stringstream is(sLine);
        
            float temp;
            cols_previous = cols_current;
            cols_current = 0;
            while (is >> temp)
            {
                cols_current++;
            }
            if(cols_current == cols_previous)
            {
                number_columns = cols_current;
                break;
            }
            header_lines++;
        }
    }
    
    infile.close();
    
    return header_lines;
}
