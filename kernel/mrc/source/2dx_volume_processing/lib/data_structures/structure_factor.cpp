/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "structure_factor.hpp"
#include "volume2dx.hpp"

namespace ds = volume::data;

ds::StructureFactors::StructureFactors(double min_resolution, double max_resolution, int resolution_bins)
{
    _min_resolution = min_resolution;
    _max_resolution = max_resolution;
    _resolution_bins = resolution_bins;
}

void ds::StructureFactors::initialize_intensities(const volume::data::Volume2dx& volume)
{
    
    Volume2dx copied_volume(volume.header());
    copied_volume.set_data(volume);
    
    //Get the Fourier data
    FourierSpaceData fourier_data = copied_volume.get_fourier();
    
    //Initialize the intensities
    _intensities = (double *) calloc(_resolution_bins, sizeof(double));
    int* spot_counter = (int*) calloc(_resolution_bins, sizeof(int));
    
    //Iterate over all possible h,k,l and populate intensity sums
    for(FourierSpaceData::const_iterator itr=fourier_data.begin(); itr!=fourier_data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        DiffractionSpot spot = (*itr).second;
        if ( index.h() != 0 || index.k() != 0 || index.l() != 0 ) 
        {
            double resolution = 1 / copied_volume.resolution_at(index.h(), index.k(), index.l());
            
            //Find the appropriate bin
            if ( resolution <= max_resolution() && resolution >= min_resolution() ) 
            {
                int bin = get_resolution_bin(resolution);
                _intensities[bin] += pow(spot.value().amplitude(), 2);
                spot_counter[bin]++;
            }
        }
    }
    
    //Average the output
    for(int bin=0; bin<_resolution_bins; bin++)
    {
        if(spot_counter[bin] !=0 ) _intensities[bin] /= spot_counter[bin];
        else _intensities[bin] = 0.0;
    }
    
    
}

std::string ds::StructureFactors::to_string() const
{
    std::string output = "";
    output += "\n";
    output += "Structure factors in range (" + 
                std::to_string(min_resolution()) +
                ", " + std::to_string(max_resolution()) +
                ") spaced by " + std::to_string(resolution_spacing())+ ":\n\n";
    
    for(int bin=0; bin<resolution_bins(); bin++)
    {
        double resolution = min_resolution() + (bin+1)*resolution_spacing();
        double intensity = _intensities[bin];
        output += std::to_string(resolution) + "\t" + std::to_string(intensity) + "\n";
    }
    
    return output;
}

std::string ds::StructureFactors::plot_profile() const
{
    std::string output = "";
    output += "\n";
    output += "Structure factors in range (" + 
                std::to_string(min_resolution()) +
                ", " + std::to_string(max_resolution()) +
                ") spaced by " + std::to_string(resolution_spacing())+ ":\n\n";
    
    double max_inten = max_intensity();
    double max_points = 100;
    double step_value = max_inten/max_points;
    
    for(int bin=0; bin<resolution_bins(); bin++)
    {
        double resolution = min_resolution() + (bin+1)*resolution_spacing();
        double intensity = _intensities[bin];
        int steps = int(intensity/step_value);
        output += std::to_string(resolution) + "\t|";
        for(int s=0; s<steps; s++) output += '+';
        output += " (" + std::to_string(intensity) + ")";
        output +="\n";
    }
    
    return output;
}

double ds::StructureFactors::intensity_at(double resolution) const
{
    if(!initialized())
    {
        std::cerr << "ERROR while fetching intensity from radial profile:\n"
                  << "The intensities were not set!! Did you forget to initialize them?\n";
        exit(1);
    }
    
    int bin = get_resolution_bin(resolution);
    
    if(bin != -1)
        return _intensities[bin];
    else
        return -1.0;
            
}

double ds::StructureFactors::min_resolution() const
{
    return _min_resolution;
}

double ds::StructureFactors::max_resolution() const
{
    return _max_resolution;
}

int ds::StructureFactors::resolution_bins() const
{
    return _resolution_bins;
}

double ds::StructureFactors::resolution_spacing() const
{
    return (max_resolution() - min_resolution())/(resolution_bins());
}

double ds::StructureFactors::max_intensity() const
{
    if(!initialized())
    {
        return 0.0;
    }
    
    double max = 0.0;
    for(int bin=0; bin<resolution_bins(); bin++)
    {
        if(_intensities[bin] >  max) max = _intensities[bin];
    }
    
    return max;
    
}

bool ds::StructureFactors::initialized() const
{
    if(_intensities == NULL)
        return false;
    else
        return true;
}

int ds::StructureFactors::get_resolution_bin(double resolution) const
{
    int bin = floor((resolution-min_resolution())/resolution_spacing());
    if( bin>=0 && bin<resolution_bins() )
        return bin;
    else
        return -1;
}
