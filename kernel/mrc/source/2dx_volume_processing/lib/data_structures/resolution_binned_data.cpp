/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "resolution_binned_data.hpp"
#include "../utilities/filesystem.hpp"

namespace ds = tdx::data;

ds::ResolutionBinnedData::ResolutionBinnedData(double min_resolution, double max_resolution, int resolution_bins)
{
    _min_resolution = min_resolution;
    _max_resolution = max_resolution;
    _resolution_bins = resolution_bins;
    _data = (double*) calloc(_resolution_bins, sizeof(double));
    _counts = (int*) calloc(_resolution_bins, sizeof(int));
}

void ds::ResolutionBinnedData::add_data_at(double resolution, double value)
{
    //Find the appropriate bin
    if ( resolution <= max_resolution() && resolution >= min_resolution() ) 
    {
        int bin = get_resolution_bin(resolution);
        if(bin != -1)
        {   
            _data[bin] += value;
            _counts[bin]++;
        }
    }
} 

void ds::ResolutionBinnedData::set_bin_sum(int bin, double sum)
{
    if(bin >=0 && bin < resolution_bins()) _data[bin] = sum;
    else std::cerr << "Warning: The bin provided exceeds limits.";
}

void ds::ResolutionBinnedData::set_bin_count(int bin, int count)
{
    if(bin >=0 && bin < resolution_bins()) _counts[bin] = count;
    else std::cerr << "Warning: The bin provided exceeds limits.";
}

void ds::ResolutionBinnedData::write_sum(std::string file) const
{
    write(file, false);
}

void ds::ResolutionBinnedData::write_average(std::string file) const
{
    write(file, true);
}

void ds::ResolutionBinnedData::write(std::string file, bool average) const
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
    output += "data in range (" + 
                std::to_string(min_resolution()) +
                ", " + std::to_string(max_resolution()) +
                ") spaced by " + std::to_string(resolution_spacing())+ ":\n\n";
    
    for(int bin=0; bin<resolution_bins(); bin++)
    {
        double resolution = min_resolution() + (bin+1)*resolution_spacing();
        double data;
        if(average) data = average_in(bin);
        else data = sum_in(bin);
        output += std::to_string(resolution) + "\t" + std::to_string(data) + "\n";
    }
    
    writeFile << output;
    writeFile.close();
}

std::string ds::ResolutionBinnedData::plot_sum() const
{
    return plot_profile(false);
}

std::string ds::ResolutionBinnedData::plot_average() const
{
    return plot_profile(true);
}

std::string ds::ResolutionBinnedData::plot_profile(bool average) const
{
    std::string output = "";
    output += "\n";
    if(average) output += "#Averaged ";
    else output += "#Summed ";
    output += "data in range (" + 
                std::to_string(min_resolution()) +
                ", " + std::to_string(max_resolution()) +
                ") spaced by " + std::to_string(resolution_spacing())+ ":\n\n";
    
    double max_value;
    if(average) 
    {
        max_value = max_averaged_value();
    }
    else 
    {
        max_value = max_summed_value();
    }
    
    double max_points = 100;
    double step_value = max_value/max_points;
    
    for(int bin=0; bin<resolution_bins(); bin++)
    {
        double resolution = min_resolution() + (bin+1)*resolution_spacing();
        
        double data;
        if(average) data = average_in(bin);
        else data = sum_in(bin);
        
        int steps = int(data/step_value);
        output += std::to_string(resolution) + "\t|";
        for(int s=0; s<steps; s++) output += '+';
        output += " (" + std::to_string(data) + ")";
        output +="\n";
    }
    
    return output;
}

double ds::ResolutionBinnedData::sum_at(double resolution) const
{   
    return average_in(get_resolution_bin(resolution));      
}

double ds::ResolutionBinnedData::average_at(double resolution) const
{   
    return average_in(get_resolution_bin(resolution));       
}

double ds::ResolutionBinnedData::sum_in(int bin) const
{
    if(bin >= 0 && bin < resolution_bins())
        return _data[bin];
    else
        return -1.0;
}

double ds::ResolutionBinnedData::average_in(int bin) const
{
    if(bin >= 0 && bin < resolution_bins())
        if(_counts[bin] == 0) return 0.0;
        else return _data[bin]/_counts[bin];
    else
        return -1.0;
}

double ds::ResolutionBinnedData::min_resolution() const
{
    return _min_resolution;
}

double ds::ResolutionBinnedData::max_resolution() const
{
    return _max_resolution;
}

int ds::ResolutionBinnedData::resolution_bins() const
{
    return _resolution_bins;
}

double ds::ResolutionBinnedData::resolution_spacing() const
{
    return (max_resolution() - min_resolution())/(resolution_bins());
}

double ds::ResolutionBinnedData::max_summed_value() const
{
    double max = 0.0;
    for(int bin=0; bin<resolution_bins(); bin++)
    {
        if(sum_in(bin) >  max) max = sum_in(bin);
    }
    
    return max;  
}

double ds::ResolutionBinnedData::max_averaged_value() const
{
    double max = 0.0;
   for(int bin=0; bin<resolution_bins(); bin++)
    {
        if(average_in(bin) >  max) max = average_in(bin);
    }
    
    return max;
}

int ds::ResolutionBinnedData::get_resolution_bin(double resolution) const
{
    int bin = floor((resolution-min_resolution())/resolution_spacing());
    if( bin>=0 && bin<resolution_bins() )
        return bin;
    else
        return -1;
}