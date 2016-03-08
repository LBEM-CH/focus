#include "binary_file.hpp"

using namespace tdx;

BinaryFile::BinaryFile(const std::string& file_name, openmode mode) 
    : File(file_name, mode | std::ios_base::binary)
{

}

std::string BinaryFile::read_string(const int& read_size)
{
    char* temp = new char[read_size];
    this->read(temp, read_size*sizeof(char));
    
    std::string value = std::string(temp);
    return value;
}

int BinaryFile::read_int()
{
    int value;
    this->read((char*)&value, sizeof(int));
    
    return value;
}

float BinaryFile::read_float()
{
    float value;
    this->read((char*)&value, sizeof(float));
    
    return value;
}