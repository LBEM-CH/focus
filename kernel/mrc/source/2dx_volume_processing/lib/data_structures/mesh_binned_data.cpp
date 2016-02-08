/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "mesh_binned_data.hpp"
#include "../utilities/filesystem.hpp"

namespace ds = tdx::data;

ds::MeshBinnedData::MeshBinnedData(double min_x, double max_x, double min_y, double max_y, int bins_x, int bins_y)
{
    _min_x = min_x;
    _max_x = max_x;
    _min_y = min_y;
    _max_y = max_y;
    _bins_x = bins_x;
    _bins_y = bins_y;
    _data = (double*) calloc(_bins_x*_bins_y, sizeof(double));
    _counts = (int*) calloc(_bins_x*_bins_y, sizeof(int));
}

void ds::MeshBinnedData::add_data_at(double x, double y, double value)
{
    //Find the appropriate bin
    if ( x <= max_x() && x >= min_x() && y <= max_y() && y >= min_y()) 
    {
        int bin_x = get_bin_x(x);
        int bin_y = get_bin_y(y);
        if(bin_x != -1 && bin_y !=-1)
        {   
            int bin = get_linear_bin(bin_x, bin_y);
            //std::cout << "x: " << x << " y: " << y << " falls in: " << bin_x << ", " << bin_y << " -> " << bin << " with value: " << value << "\n"; 
            _data[bin] += value;
            _counts[bin]++;
        }
    }
}

void ds::MeshBinnedData::set_bin_sum(int bin_x, int bin_y, double sum)
{
    if(bin_x >=0 && bin_x < bins_x() && bin_y >=0 && bin_y < bins_y()) _data[get_linear_bin(bin_x, bin_y)] = sum;
    else std::cerr << "Warning: The bin provided exceeds limits.";
}

void ds::MeshBinnedData::set_bin_count(int bin_x, int bin_y, int count)
{
    if(bin_x >=0 && bin_x < bins_x() && bin_y >=0 && bin_y < bins_y()) _counts[get_linear_bin(bin_x, bin_y)] = count;
    else std::cerr << "Warning: The bin provided exceeds limits.";
}

void ds::MeshBinnedData::write_sum(std::string file) const
{
    write(file, false);
}

void ds::MeshBinnedData::write_average(std::string file) const
{
    write(file, true);
}

void ds::MeshBinnedData::write(std::string file, bool average) const
{
    //Check for the existence of the file
    if(tdx::utilities::filesystem::FileExists(file))
    {
        std::cout << "WARNING: File.. " << file << " already exists. Overwriting!\n";
    }
    
    std::ofstream writeFile(file);
    
    std::string output = "";
    output += "\n";
    if(average) output += "#Averaged ";
    else output += "#Summed ";
    output += "data in range: X direction (" + 
                std::to_string(min_x()) +
                ", " + std::to_string(max_x()) +
                ") and in Y direction (" + 
                std::to_string(min_y()) +
                ", " + std::to_string(max_y()) +
                ") spaced in X by " + std::to_string(spacing_x())+
                " and in Y by " + std::to_string(spacing_y()) + "\n\n";
    
    for(int bin_x=0; bin_x<bins_x(); bin_x++)
    {
        for(int bin_y=0; bin_y<bins_y(); bin_y++)
        {
            double x = min_x() + (bin_x)*spacing_x();
            double y = min_y() + (bin_y)*spacing_y();
            double data;
            if(average) data = average_in(bin_x, bin_y);
            else data = sum_in(bin_x, bin_y);
            output += std::to_string(x) + "\t" + std::to_string(y) + "\t" + std::to_string(data) + "\n";
        }
    }
    
    writeFile << output;
    writeFile.close();
}

double ds::MeshBinnedData::sum_at(double x, double y) const
{   
    return sum_in(get_bin_x(x), get_bin_y(y));      
}

double ds::MeshBinnedData::average_at(double x, double y) const
{   
    return average_in(get_bin_x(x), get_bin_y(y));       
}

double ds::MeshBinnedData::sum_in(int bin_x, int bin_y) const
{
    if(bin_x >=0 && bin_x < bins_x() && bin_y >=0 && bin_y < bins_y())
        return _data[get_linear_bin(bin_x, bin_y)];
    else
        return -1.0;
}

double ds::MeshBinnedData::average_in(int bin_x, int bin_y) const
{
    if(bin_x >=0 && bin_x < bins_x() && bin_y >=0 && bin_y < bins_y())
        if(_counts[get_linear_bin(bin_x, bin_y)] == 0) return 0.0;
        else return _data[get_linear_bin(bin_x, bin_y)]/_counts[get_linear_bin(bin_x, bin_y)];
    else
        return -1.0;
}

double ds::MeshBinnedData::min_x() const
{
    return _min_x;
}

double ds::MeshBinnedData::max_x() const
{
    return _max_x;
}

double ds::MeshBinnedData::min_y() const
{
    return _min_y;
}

double ds::MeshBinnedData::max_y() const
{
    return _max_y;
}

int ds::MeshBinnedData::bins_x() const
{
    return _bins_x;
}

int ds::MeshBinnedData::bins_y() const
{
    return _bins_y;
}

double ds::MeshBinnedData::spacing_x() const
{
    return (max_x() - min_x())/(bins_x());
}

double ds::MeshBinnedData::spacing_y() const
{
    return (max_y() - min_y())/(bins_y());
}

int ds::MeshBinnedData::get_bin_x(double x) const
{
    int bin = floor((x-min_x())/spacing_x());
    if( bin>=0 && bin<bins_x() )
        return bin;
    else
        return -1;
}

int ds::MeshBinnedData::get_bin_y(double y) const
{
    int bin = floor((y-min_y())/spacing_y());
    if( bin>=0 && bin<bins_y() )
        return bin;
    else
        return -1;
}

int ds::MeshBinnedData::get_linear_bin(int bin_x, int bin_y) const
{
    return bin_y + bins_y() * bin_x;
}