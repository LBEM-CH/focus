/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "mtz_io.hpp"

#include "../utilities/binary_file_utilities.hpp"
#include "../utilities/filesystem.hpp"
#include "../utilities/angle_utilities.hpp"
#include "2dx_volume_processing/lib/utilities/string_utilities.hpp"

namespace io = tdx::io;

std::string tdx::io::MTZUtils::file_name()
{
    return _file_name;
}

tdx::io::MTZUtils::MTZUtils(std::string file_name)
{
    _file_name = file_name;
    
    //Check for the presence of file
    if (!tdx::utilities::filesystem::FileExists(file_name)){
        std::cerr << "File not found: " << file_name << std::endl;
        exit(1);
    }
    
    namespace bf = tdx::utilities::binary_file_utilities;
    
    std::ifstream file(file_name, std::ios::in|std::ios::binary);
    file.seekg (0, std::ios::beg);
    
    //Check if the first record is called MTZ
    std::string first_record = bf::read_string(file, 4);
    if(first_record.substr(0,3) != "MTZ")
    {
        std::cerr << "The file is not supposed to be in MTZ format";
        exit(1);
    }
    
    //Get the header location
    _header_position = bf::read_int(file);
    
    std::cout << "Header location: " << _header_position <<"\n";
    
    file.close();
    
    read_header();

}

std::string tdx::io::MTZUtils::header_string()
{
    std::string output = "\n";
    if(file_name() != "") output += "Origin file name: " + file_name() + "\n";
    if(_title != "" ) output += "Title: " + _title + "\n\n";
    
    output += "MTZ Header Information:\n";
    output += "\t|Number of Columns: " + std::to_string(_number_of_columns) + "\n";
    output += "\t|Number of Reflections: " + std::to_string(_number_of_reflections) + "\n";
    output += "\t|Cell: ";
    for(int i=0; i<6; i++) output += std::to_string(_cell[i]) + "  ";
    output += "\n";
    output += "\t|Resolution: ";
    for(int i=0; i<2; i++) output += std::to_string(_resolution[i]) + "  ";
    output += "\n";
    output+= "\t|Columns: \n";
    for(int i=0; i< _column_labels.size(); i++)  
            output += "\t\t" + std::to_string(i+1) + ": " + _column_labels[i] + "  " + _column_type[i] + " " + std::to_string(_column_min[i]) + " " + std::to_string(_column_max[i]) + "\n";
    
    return output;
}

void tdx::io::MTZUtils::read_header() 
{
    std::ifstream file(file_name(), std::ios::in|std::ios::binary);
    file.seekg (_header_position, std::ios::beg);
    
    long file_size = tdx::utilities::binary_file_utilities::file_size(file_name());
    int number_header_lines = (int)(file_size - _header_position)/80;
    
    std::cout << "Number of header lines found: " << number_header_lines <<"\n";
    
    //bool reading_history = false;
    std::cout << "Reading record ";
    for(int l=0; l<number_header_lines; l++)
    {
        std::string line = tdx::utilities::binary_file_utilities::read_string(file, 80);
        std::string trimmed = tdx::utilities::string_utilities::trim(line);
        
        std::vector<std::string> elems = tdx::utilities::string_utilities::split(trimmed, ' ');
        
        if(elems.size() > 0)
        {
            try
            {
                if(elems.at(0) == "VERSION")
                {
                    std::cout << "VERSION.. ";
                    std::cout << trimmed << "\n";
                    std::string version = elems.at(1);
                    if(version!= "MTZ:V1.1" ) 
                    {
                        std::cerr << "Incompatible version of MTZ file\n";
                        exit(1);
                    }
                }

                else if(elems.at(0).substr(0,4) == "TITL")
                {
                    std::cout << "TITLE.. ";
                    size_t beg_pos = line.find_first_of('E') + 2;
                    _title = line.substr(beg_pos, line.length() - beg_pos);
                }

                else if(elems.at(0).substr(0,4) == "NCOL" && elems.size() >= 3)
                {
                    std::cout << "NCOL.. ";
                    _number_of_columns = std::stoi(elems.at(1));
                    _number_of_reflections = std::stoi(elems.at(2));
                }

                else if(elems.at(0).substr(0,4) == "CELL" && elems.size() >= 7)
                {
                    std::cout << "CELL.. ";
                    for(int i=0; i<6; i++) _cell[i] = std::stod(elems.at(i+1));
                }

                else if(elems.at(0).substr(0,4) == "RESO" && elems.size() >= 3)
                {
                    std::cout << "RESO.. ";
                    _resolution[0] = std::stod(elems.at(1));
                    _resolution[1] = std::stod(elems.at(2));
                }

                else if(elems.at(0).find("COLU") != std::string::npos && elems.size() >= 5)
                {
                    std::cout << "COLUMN.. ";
                    _column_labels.push_back(elems.at(1));
                    _column_type.push_back(elems.at(2));
                    _column_min.push_back(std::stod(elems.at(3)));
                    _column_max.push_back(std::stod(elems.at(4)));
                }

                else if(elems.at(0).substr(0,3) == "END")
                {
                    break;
                }
            }
            catch(const std::exception& e)
            {
                std::cerr << "\nError in reading MTZ file header from:\n\t" << file_name() << std::endl;
                std::cerr << e.what() << "\n";
                exit(1);
            }
        }
        
        
    }
    
    std::cout <<" FINISHED !! \n";
    
    file.close();

}