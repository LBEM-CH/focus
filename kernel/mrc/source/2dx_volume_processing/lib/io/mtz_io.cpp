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

tdx::data::ReflectionData tdx::io::MTZUtils::data()
{
    return _data;
}

tdx::data::VolumeHeader tdx::io::MTZUtils::header() 
{
    tdx::data::VolumeHeader hdr((int)_cell[0], (int)_cell[1], (int)_cell[2]);
    hdr.set_file_name(file_name());
    hdr.set_title(_title);
    hdr.set_gamma(_cell[5]);

    return hdr;
}


tdx::io::MTZUtils::MTZUtils(std::string file_name)
{
    _file_name = file_name;
    _data = tdx::data::ReflectionData();
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
    _header_position = (size_t) bf::read_int(file);
    
    std::cout << "Header location: " << _header_position <<"\n";
    
    file.close();
    
    read_header();
    
    read_data();

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

void tdx::io::MTZUtils::read_data()
{   
    std::cout << "Reading data.. \n";
    
    int col_order[6] = {-1}; // Columns number for H, K, L, AMP, PHASE, FOM respectively;
    
    for(int col=0; col<_number_of_columns; col++)
    {
        if     (_column_labels[col].at(0) == 'H' && _column_type[col].at(0) == 'H') col_order[0] = col;
        else if(_column_labels[col].at(0) == 'K' && _column_type[col].at(0) == 'H') col_order[1] = col;
        else if(_column_labels[col].at(0) == 'L' && _column_type[col].at(0) == 'H') col_order[2] = col;
        else if(_column_labels[col].at(0) == 'F' && _column_type[col].at(0) == 'F') col_order[3] = col;
        else if(_column_labels[col].at(0) == 'P' && _column_type[col].at(0) == 'P') col_order[4] = col;
        else if(_column_labels[col] == "FOM"     && _column_type[col].at(0) == 'W') col_order[5] = col;
        else std::cout << "WARNING: Ignoring column with label: " << _column_labels[col] << " and type: " << _column_type[col] <<"\n";
    }
    
    std::cout << "Expected order: ";
    for(int i=0; i<5; i++)
    {
        std::cout << col_order[i] << " ";
        if(col_order[i] == -1)
        {
            std::cerr << "One of the essential columns was missing while reading MTZ file\n";
            exit(1);
        }
    }
    std::cout <<"\n";
    
    if(_number_of_reflections*_number_of_columns+21 > _header_position)
    {
        std::cerr << "Number of reflections present are less than expected.\n";
        exit(1);
    }
    
    
    std::ifstream file(file_name(), std::ios::in|std::ios::binary);
    file.seekg (20*sizeof(float), std::ios::beg);
    
    
    _data.clear();
    
    float* read_reflection = new float[_number_of_columns]();
    for(size_t ref=0; ref<_number_of_reflections; ref++)
    {
        try
        {
            
            
            for(int col=0; col<_number_of_columns; col++)
            {
                read_reflection[col] = tdx::utilities::binary_file_utilities::read_float(file);   
            }
            int h = (int) read_reflection[col_order[0]];
            int k = (int) read_reflection[col_order[1]];
            int l = (int) read_reflection[col_order[2]];
            double amp = (double) read_reflection[col_order[3]];
            double phase = (double) read_reflection[col_order[4]];
            double fom = 1.0;
            if(col_order[5] >= 0) fom = (double) read_reflection[col_order[5]];
            if(fom > 1.0) fom = fom * 0.01;
        
        
            tdx::data::Complex value(amp * cos(phase), amp*sin(phase));
        
            _data.set_spot_at(h,k,l, value, fom);
            
        }
        catch(const std::exception& e)
        {
            std::cerr << "\nError in reading MTZ file reflection:\t" << ref << std::endl;
            std::cerr << e.what() << "\n";
        }
        
    }
    delete read_reflection;
    file.close();
            
}

void tdx::io::MTZUtils::read_header() 
{
    std::ifstream file(file_name(), std::ios::in|std::ios::binary);
    file.seekg ((_header_position-1)*sizeof(float), std::ios::beg);
    
    long file_size = tdx::utilities::binary_file_utilities::file_size(file_name());
    int number_header_lines = (int)(file_size - (_header_position-1)*sizeof(float))/80;
    
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
                if(elems.at(0).substr(0,4) == "VERS")
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
                    _number_of_columns = (size_t)std::stoi(elems.at(1));
                    _number_of_reflections = (size_t)std::stoi(elems.at(2));
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
    
    if(_number_of_columns != _column_labels.size() || _number_of_columns != _column_type.size())
    {
        std::cerr << "Error while reading the MTZ file. Column counts do not match.\n";
        exit(1);
    }
    
    std::cout <<" FINISHED !! \n";
    
    file.close();

}