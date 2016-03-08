/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "mtz_io.hpp"

#include "../basics/string.hpp"
#include "../basics/binary_file.hpp"

#include "../utilities/angle_utilities.hpp"

std::string tdx::io::MTZParser::file_name()
{
    return _file_name;
}

tdx::data::ReflectionData tdx::io::MTZParser::data()
{
    return _data;
}

tdx::data::VolumeHeader tdx::io::MTZParser::header() 
{
    tdx::data::VolumeHeader hdr((int)_cell[0], (int)_cell[1], (int)_cell[2]);
    hdr.set_file_name(file_name());
    hdr.set_title(_title);
    hdr.set_gamma(_cell[5]*M_PI/180);

    return hdr;
}


tdx::io::MTZParser::MTZParser(std::string file_name)
{
    std::cout << "Opening in READ mode: " << file_name << "\n";
    _file_name = file_name;
    _data = tdx::data::ReflectionData();
    
    tdx::BinaryFile infile(file_name, tdx::File::in);
    //Check for the presence of file
    if (!infile.exists())
    {
        std::cerr << "File not found: " << file_name << std::endl;
        exit(1);
    }
    
    infile.seekg (0, std::ios::beg);
    
    //Check if the first record is called MTZ
    std::string first_record = infile.read_string(4);
    if(first_record.substr(0,3) != "MTZ")
    {
        std::cerr << "The file is not supposed to be in MTZ format\n";
        exit(1);
    }
    
    //Get the header location
    _header_position = (size_t) infile.read_int();
    
    std::cout << "Header location: " << _header_position <<"\n";
    
    infile.close();
    
    read_header();
    
    read_data();

}

tdx::io::MTZParser::MTZParser(std::string file_name, tdx::data::ReflectionData data, tdx::data::VolumeHeader header, int number_of_columns) 
{
    std::cout << "Opening in WRITE mode: " << file_name << "\n";
    
    _file_name = file_name;
    _data = data;
    
    tdx::BinaryFile infile(file_name, tdx::File::in);
    
    //Check for the presence of file
    if (!infile.exists())
    {
        std::cerr << "File not found: " << file_name << std::endl;
        exit(1);
    }
    
    //Check number of columns
    if(number_of_columns < 5 || number_of_columns > 7)
    {
        if(number_of_columns < 5) number_of_columns = 5;
        if(number_of_columns > 7) number_of_columns = 7;
        std::cerr << "WARNING: Can only write 5,6 or 7 columns to MTZ file.\n";
        std::cerr << "WARNING: Setting write columns to: " << number_of_columns << "\n";
    }
    
    _number_of_columns = number_of_columns;
    _number_of_reflections = data.spots();
    _header_position = _number_of_columns*_number_of_reflections + 21;
    _cell[0] = (float) header.xlen(); 
    _cell[1] = (float) header.ylen();
    _cell[2] = (float) header.zlen();
    _cell[3] = 90.00;
    _cell[4] = 90.00;
    _cell[5] = (float) header.gamma()*180/M_PI;
    _title = header.title();
    
    _column_labels.clear();
    _column_type.clear();
    _column_min.clear();
    _column_max.clear();
    
    
    _column_labels = {"H", "K", "L", "FC", "PHIC"};
    _column_type = {'H', 'H', 'H', 'F', 'P'};
    _column_min = {0, 0, 0, 0, 0};
    _column_max = {0, 0, 0, 0, 0};
    
    if(number_of_columns >= 6)
    {
        _column_labels.push_back("FOM");
        _column_type.push_back('W');
        _column_min.push_back(0.0);
        _column_max.push_back(0.0);
    }
    
    if(number_of_columns == 7)
    {
        _column_labels.push_back("SIGF");
        _column_type.push_back('Q');
        _column_min.push_back(1.0);
        _column_max.push_back(1.0);
    }
}


std::string tdx::io::MTZParser::header_string()
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

void tdx::io::MTZParser::read_data()
{   
    std::cout << "Reading data.. \n";
    
    int col_order[6] = {-1}; // Columns number for H, K, L, AMP, PHASE, FOM respectively;
    
    for(int col=0; col<_number_of_columns; col++)
    {
        if     (_column_labels[col].at(0) == 'H' && _column_type[col] == 'H') col_order[0] = col;
        else if(_column_labels[col].at(0) == 'K' && _column_type[col] == 'H') col_order[1] = col;
        else if(_column_labels[col].at(0) == 'L' && _column_type[col] == 'H') col_order[2] = col;
        else if(_column_labels[col].at(0) == 'F' && _column_type[col] == 'F') col_order[3] = col;
        else if(_column_labels[col].at(0) == 'P' && _column_type[col] == 'P') col_order[4] = col;
        else if(_column_labels[col] == "FOM"     && _column_type[col] == 'W') col_order[5] = col;
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
    
    
    tdx::BinaryFile infile(file_name(), tdx::File::in);
    infile.seekg (20*sizeof(float), std::ios::beg);
    
    
    _data.clear();
    
    float* read_reflection = new float[_number_of_columns]();
    for(size_t ref=0; ref<_number_of_reflections; ref++)
    {
        try
        {
            
            
            for(int col=0; col<_number_of_columns; col++)
            {
                read_reflection[col] = infile.read_float();   
            }
            int h = (int) read_reflection[col_order[0]];
            int k = (int) read_reflection[col_order[1]];
            int l = (int) read_reflection[col_order[2]];
            double amp = (double) read_reflection[col_order[3]];
            double phase = (double) read_reflection[col_order[4]]*M_PI/180;
            double fom = 1.0;
            if(col_order[5] >= 0) fom = (double) read_reflection[col_order[5]];
            if(fom > 1.0) fom = fom * 0.01;
        
            //For negative h use Friedel spot
            if(h < 0)
            {
                h=-1*h; k=-1*k; l=-1*l; phase=-1*phase;
            }
            tdx::Complex value(amp * cos(phase), amp*sin(phase));
        
            _data.set_spot_at(h,k,l, value, fom);
            
        }
        catch(const std::exception& e)
        {
            std::cerr << "\nError in reading MTZ file reflection:\t" << ref << std::endl;
            std::cerr << e.what() << "\n";
        }
        
    }
    delete read_reflection;
    infile.close();
            
}

void tdx::io::MTZParser::read_header() 
{
    tdx::BinaryFile infile(file_name(), tdx::File::in);
    infile.seekg ((_header_position-1)*sizeof(float), std::ios::beg);
    
    size_t file_size = infile.file_size();
    int number_header_lines = (int)(file_size - (_header_position-1)*sizeof(float))/80;
    
    std::cout << "Number of header lines found: " << number_header_lines <<"\n";
    
    //bool reading_history = false;
    std::cout << "Reading record ";
    for(int l=0; l<number_header_lines; l++)
    {
        std::string line = infile.read_string(80);
        std::string trimmed = tdx::String::trim(line);
        //std::cout << "\n" <<line << "\n";
        std::vector<std::string> elems = tdx::String::split(trimmed, ' ');
        
        if(elems.size() > 0)
        {
            try
            {
                if(elems.at(0).substr(0,4) == "VERS")
                {
                    std::cout << "VERSION.. ";
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

                else if(elems.at(0).substr(0,4) == "COLU" && elems.size() >= 6)
                {
                    std::cout << "COLUMN.. ";
                    _column_labels.push_back(elems.at(1));
                    _column_type.push_back(elems.at(2).at(0));
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
    
    infile.close();

}

void tdx::io::MTZParser::write()
{   
    std::cout << "Opening file for writing:\n"; 
    std::ofstream file(file_name(), std::ios::out|std::ios::binary);
    
    file.write("MTZ ", sizeof(float));
    file.write((char*) &_header_position, sizeof(float));
    
    //Write file architecture
    /* Machine stamp is quite a complex routine
     * It also considers the byte order into consideration. 
     * But for the sake of simplicity and as we assume that the files
     * written on same machine would be read by same machine (NATIVE
     * ENVIRONMENT) we set all chars in the machine stamp to 0. This is 
     * caught in the CCP4 code and converted to native values.
     * mtstring[0] is float byte order
     * mtstring[1] is integer byte order
     * mtstring[3] and mtstring[4] are anyways 0
     */
    unsigned char mtstring[4];
    unsigned int iconvert = 0;
    unsigned int fconvert = 0;
    
    mtstring[0] = fconvert | (fconvert << 4);
    mtstring[1] = 1 | (iconvert << 4);
    mtstring[2] = mtstring[3] = 0;
    
    file.write((const char*)&mtstring, 4*sizeof(char));
    
    //Write reflections
    file.seekp(20*sizeof(float), std::ios::beg);
    
    for(tdx::data::ReflectionData::const_iterator itr = _data.begin(); itr != _data.end(); itr++)
    {
        int h = (*itr).first.h();
        int k = (*itr).first.k();
        int l = (*itr).first.l();
        float amp = (float) (*itr).second.amplitude();
        float phase = (float) (*itr).second.phase();
        float fom = (float) (*itr).second.weight()*100;
        float sigf = 1.0;
        
        //Get to positive l
        if(l < 0)
        {
             h=-1*h; k=-1*k; l=-1*l; phase=-1*phase;
        }
        
        phase = (float)tdx::utilities::angle_utilities::CorrectRadianPhase((double)phase);
        phase = phase*180/M_PI;
        
        try
        {
            file.write((const char *) &h, sizeof(int));
            file.write((const char *) &k, sizeof(int));
            file.write((const char *) &l, sizeof(int));
            file.write((const char *) &amp, sizeof(float));
            file.write((const char *) &phase, sizeof(float));
            if(_number_of_columns >=6 ) file.write((const char *) &fom, sizeof(float));
            if(_number_of_columns >=7 ) file.write((const char *) &sigf, sizeof(float));
        }
        catch(const std::exception& e)
        {
            std::cerr << "\nError in writing MTZ file reflection:\t" << h << " " << k << " " << l << std::endl;
            std::cerr << e.what() << "\n";
        }

        //Correct the min and max
        if(h < _column_min[0]) _column_min[0] = h;
        if(k < _column_min[1]) _column_min[1] = k;
        if(l < _column_min[2]) _column_min[2] = l;
        if(amp < _column_min[3]) _column_min[3] = amp;
        if(phase < _column_min[4]) _column_min[4] = phase;
        if(_number_of_columns >=6 ) if(fom < _column_min[5]) _column_min[5] = fom;
        
        if(h > _column_max[0]) _column_max[0] = h;
        if(k > _column_max[1]) _column_max[1] = k;
        if(l > _column_max[2]) _column_max[2] = l;
        if(amp > _column_max[3]) _column_max[3] = amp;
        if(phase > _column_max[4]) _column_max[4] = phase;
        if(_number_of_columns >=6 ) if(fom > _column_max[5]) _column_max[5] = fom;
        
    }
    
    std::cout << "Written the data\n";
    //Write header
    file.seekp((_header_position-1)*sizeof(float), std::ios::beg);
    
    file.write("VERS MTZ:V1.1", 80*sizeof(char));
    
    if(_title.length() > 70) _title = _title.substr(0,70);
    file.write(std::string("TITLE "+_title).c_str(), 80*sizeof(char));
    
    //NCOL %8d %12d %8d
    int zero = 0;
    file.write(std::string("NCOL " + tdx::String::string_of(_number_of_columns, 8) + " " 
                + tdx::String::string_of(_number_of_reflections, 12) + " " + tdx::String::string_of(zero, 8)).c_str(), 80*sizeof(char));
    
    //CELL  %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f
    std::string cell_str = "CELL ";
    for(int i=0; i<6; i++) cell_str += " " + tdx::String::string_of(_cell[i], 9, 4); 
    file.write(cell_str.c_str(), 80*sizeof(char));
    
    //COLUMN %30s %c %17.9g %17.9g %4d
    for(int col=0; col < _number_of_columns; col++)
    {
        file.write(std::string("COLUMN " + tdx::String::string_of(_column_labels[col], 30) + " " 
                + _column_type[col] + " " 
                + tdx::String::string_of(_column_min[col], 17, 9) + " "
                + tdx::String::string_of(_column_max[col], 17, 9) + " "
                + tdx::String::string_of(zero, 4)).c_str(),
                80*sizeof(char));
        
        std::time_t rawtime = std::time(0);
        struct tm * now = std::localtime(&rawtime);
        char buffer[30];
        strftime(buffer, 30, "%d/%m/%y_%I:%M:%S", now);
        file.write(std::string("COLSRC " + tdx::String::string_of(_column_labels[col], 30) +
                        " Created_" + buffer + tdx::String::string_of(zero, 4)).c_str(), 80*sizeof(char) );
    }
    
    file.write("END", 80*sizeof(char));
    file.write("MTZHIS   1", 80*sizeof(char));
    file.write("WRITTEN FROM 2dx ", 80*sizeof(char));
    file.write("MTZENDOFHEADERS ", 80*sizeof(char));
    
    std::cout << "File written\n";
    
    file.close();
}