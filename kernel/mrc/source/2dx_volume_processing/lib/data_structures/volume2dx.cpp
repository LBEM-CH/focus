/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <math.h>
#include <fftw3.h>
#include <random>

#include "volume2dx.hpp"

#include "miller_index.hpp"
#include "peak_data.hpp"

#include "../io/mrc_io.hpp"
#include "../io/reflection_io.hpp"
#include "../io/mtz_io.hpp"

#include "../symmetrization/symmetry2dx.hpp"
#include "../symmetrization/fourier_symmetrization.hpp"

#include "../utilities/angle_utilities.hpp"
#include "../utilities/filesystem.hpp"
#include "../utilities/fourier_utilities.hpp"
#include "../utilities/bead_model_generator.hpp"
#include "../utilities/number_utilities.hpp"

namespace ds = tdx::data;

ds::Volume2DX::Volume2DX(int nx, int ny, int nz)
{
    _header = ds::VolumeHeader(nx, ny, nz);
    _real = ds::RealSpaceData(nx, ny, nz);
    _fourier = ds::ReflectionData();
    _transform = tdx::transforms::FourierTransformFFTW();
    _type = NONE;
}

ds::Volume2DX::Volume2DX(const VolumeHeader& header)
{
    _header = ds::VolumeHeader(header);
    _real = ds::RealSpaceData(nx(), ny(), nz());
    _fourier = ds::ReflectionData();
    _transform = tdx::transforms::FourierTransformFFTW();
    _type = NONE;
}

ds::Volume2DX::Volume2DX(const Volume2DX& copy)
{
    _header = ds::VolumeHeader(copy.header());
    _real = ds::RealSpaceData(copy._real);
    _fourier = ds::ReflectionData(copy._fourier);
    _transform = tdx::transforms::FourierTransformFFTW(copy._transform);
    _type = copy._type;
}

ds::Volume2DX::~Volume2DX(){}

void ds::Volume2DX::reset(const Volume2DX& other)
{
    _header = ds::VolumeHeader(other._header);
    _real.reset(other._real);
    _fourier.reset(other._fourier);
    _transform.reset(other._transform);
    _type = other._type;
}

void ds::Volume2DX::clear()
{
    _real.clear();
    _fourier.clear();
    _type = NONE;
}

ds::Volume2DX& ds::Volume2DX::operator=(const Volume2DX& rhs)
{
    reset(rhs);
    return *this;
}

ds::Volume2DX ds::Volume2DX::operator+(const Volume2DX& rhs)
{
    Volume2DX added(header());
    if(rhs.has_real())
    {
        RealSpaceData rhs_real = rhs._real;
        RealSpaceData sum = get_real()+ rhs_real;
        added.set_real(sum);
    }
    else
    {
        std::cerr << "Hey, Volumes cannot be added! No real data in memory. Please set the data?";
    }
    
    return added;
}

ds::Volume2DX ds::Volume2DX::operator*(double factor)
{
    Volume2DX modified;
    if(has_real())
    {
        modified.set_real(get_real()*factor);
    }
    else if(has_fourier())
    {
        modified.set_fourier(get_fourier()*factor);
    }
    else
    {
        std::cerr << "Hey, No factor can be multiplied to volume! No data in memory. Did you forget to set the data?";
    }
    
    return modified;
}

bool ds::Volume2DX::has_fourier() const
{
    if(_type == FOURIER || _type == BOTH) return true;
    else return false;
}

bool ds::Volume2DX::has_real() const
{
    if(_type == REAL || _type == BOTH) return true;
    else return false;
}

ds::RealSpaceData ds::Volume2DX::get_real()
{
    prepare_real();
    return _real;
}

ds::ReflectionData ds::Volume2DX::get_fourier()
{
    prepare_fourier();
    return _fourier;
}

void ds::Volume2DX::prepare_fourier()
{
    if(!has_fourier()) fourier_from_real();
}

void ds::Volume2DX::prepare_real()
{
    if(!has_real()) real_from_fourier();
}

void ds::Volume2DX::set_fourier(const ReflectionData& fourier)
{
    _fourier.reset(fourier);
    _type = FOURIER;
}

void ds::Volume2DX::set_real(const RealSpaceData& real)
{
    if(real.nx() == nx() && real.ny() == ny() && real.nz() == nz())
    {
        _real.reset(real);
        _type = REAL;
    }
    else
    {
        std::cerr << "Error while setting real volume. The expected dimension are not correct.\n"
                  << "Found: " << real.nx() << " " << real.ny() << " " << real.nz() << "\n"
                  << "Expected: " << nx() << " " << ny() << " " << nz() << "\n";
        exit(1);
    }
}

void ds::Volume2DX::fourier_from_real()
{
    std::cout << "Converting the real data to Fourier. \n";
    if(_type == REAL)
    {
        _fourier.clear();
        fftw_complex* complex_data = fftw_alloc_complex(fx()*fy()*fz());
        double* real_data = _real.get_data_for_fftw();
        _transform.RealToComplex(nx(), ny(), nz(), real_data, complex_data);
        _fourier.reset_data_from_fftw(fx(), fy(), fz(), complex_data);
        fftw_free(complex_data);
        fftw_free(real_data);
        _type = BOTH;
    }
    else if(_type == NONE) 
    {
        std::cerr << "Hey, Fourier data cannot be set! Real data not in memory. Did you forget to set the data?";
    }
}

void ds::Volume2DX::real_from_fourier()
{
    std::cout << "Converting the Fourier data to real. \n";
    if(_type == FOURIER){
        double* real_data = fftw_alloc_real(nx()*ny()*nz());
        fftw_complex* complex_data = _fourier.fftw_data(fx(), fy(), fz());
        _transform.ComplexToReal(nx(), ny(), nz(), complex_data, real_data);
        _type = BOTH;
        _real.set_from_fftw(real_data);
        fftw_free(real_data);
        fftw_free(complex_data);
    }
    else if(_type == NONE) {
        std::cerr << "Hey, Real data cannot be set! Fourier data not in memory. Did you forget to set the data?";
    }
}

std::string ds::Volume2DX::to_string() const
{
    std::string output = "";
    
    output += _header.to_string();
    output += data_string();
    
    return output;
}

std::string ds::Volume2DX::data_string() const
{
    std::string output = "";
    
    output += ":\nData Information:\n";
    if(has_real())
    {
        output += ":\tReal data in memory.\n" ;
        output += ":\t|Minimum density: " + std::to_string(_real.min()) + "\n";
        output += ":\t|Maximum density: " + std::to_string(_real.max()) + "\n";
        output += ":\t|Mean density: " + std::to_string(_real.mean()) + "\n";
        output += ":\n";
    }
    
    if(has_fourier())
    {
        MillerIndex max_index = max_resolution_spot();
        output += ":\tFourier data in memory.\n" ;
        output += ":\t|Number of spots: " + std::to_string(_fourier.spots()) + "\n";
        output += ":\t|Intensity sum: " + std::to_string(_fourier.intensity_sum()) + "\n";
        output += ":\t|Spot with maximum resolution: " + max_index.to_string() + " - " + std::to_string(resolution_at(max_index.h(), max_index.k(), max_index.l())) + " A\n";
        output += ":\n";
    }
    
    if(_type == NONE)
    {
        output += ":\tNo data in memory\n";
        output += ":\n";
    }
    
    return output;
    
}

ds::VolumeHeader ds::Volume2DX::header() const {
    return _header;
}

void ds::Volume2DX::read_volume(std::string file_name, std::string format)
{
    std::cout << "Reading volume with format <"<< format << "> from file:\n\t" << file_name << "\n\n";
    if(format == "hkl")
    {
        MillerToPeakMultiMap data_read;
        tdx::io::reflection::read(file_name, 1, true, data_read);
        MillerToPeakMap averaged_data;
        tdx::utilities::fourier_utilities::average_peaks(data_read, averaged_data);
        ReflectionData fourier_data;
        fourier_data.reset(averaged_data);
        set_fourier(fourier_data);
    }
    else if(format == "hkz")
    {
        MillerToPeakMultiMap data_read;
        tdx::io::reflection::read(file_name, nz(), true, data_read);
        MillerToPeakMap averaged_data;
        tdx::utilities::fourier_utilities::average_peaks(data_read, averaged_data);
        ReflectionData fourier_data;
        fourier_data.reset(averaged_data);
        set_fourier(fourier_data);
    }
    else if (format == "mtz")
    {
        tdx::io::MTZUtils mtz(file_name);
        _header = VolumeHeader(mtz.header());
        _real.reset(RealSpaceData(nx(), ny(), nz()));
        set_fourier(mtz.data());
    }
    else if (format == "mrc" || format == "map")
    {
        _header = VolumeHeader(tdx::io::mrc::get_header(file_name, format));
        RealSpaceData read_data = tdx::io::mrc::get_data(file_name, nx(), ny(), nz());
        set_real(read_data);
    }
    else
    {
        std::cerr << "The read format <" << format << "> of file " << file_name << " not supported.\n";
    }
    
    std::cout << "Volume in memory!\n";
    
}

void ds::Volume2DX::read_volume(std::string file_name)
{
    std::string format = tdx::utilities::filesystem::FileExtension(file_name);
    
    read_volume(file_name, format);
}

void ds::Volume2DX::write_volume(std::string file_name, std::string format)
{
    std::cout << "\nWriting volume with format <"<< format << "> to file:\n\t" << file_name << "\n\n";
    if(format == "hkl")
    {
        tdx::io::reflection::write(file_name, get_fourier());
    }
    else if (format == "mrc" || format == "map")
    {
        tdx::io::mrc::write_mrc_mode_2(file_name, header(), get_real(), format);
    }
    else
    {
        std::cerr << "The write format <" << format << "> of file " << file_name << " not supported.\n";
    }
}

void ds::Volume2DX::write_volume(std::string file_name)
{
    std::string format = tdx::utilities::filesystem::FileExtension(file_name);
    write_volume(file_name, format);
}

void ds::Volume2DX::cut_xy_plane(Volume2DX& xy_plane, Volume2DX& missing_plane, int plane_number)
{
    std::cout << "Cutting the XY plane from volume.. \n";
    
    ReflectionData new_data;
    ReflectionData cut_data;
    ReflectionData current_data = get_fourier();
    
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        
        if ( index.l() == plane_number ) 
        {
            //Insert into cut_data
            cut_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
        else
        {
            //Insert into new_data
            new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
    }
    
    xy_plane = Volume2DX(nx(), ny(), 1);
    xy_plane.set_fourier(cut_data);
    
    missing_plane = Volume2DX(nx(), ny(), nz());
    missing_plane.set_fourier(new_data);
}

void ds::Volume2DX::cut_cone(Volume2DX& cone, Volume2DX& missing_cone, double cone_angle)
{
    std::cout << "Cutting the cone of " << cone_angle << " degrees from volume.. \n";
    
    double cone_angle_rad = double(cone_angle)*M_PI/180;
    
    ReflectionData new_data;
    ReflectionData cut_data;
    ReflectionData current_data = get_fourier();
    
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        double radius = std::abs(index.l()*tan(cone_angle_rad));
        double distance = sqrt(index.h()*index.h() + index.k()*index.k());
        
        if ( distance < radius ) 
        {
            //Insert into cut_data
            cut_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
        else
        {
            //Insert into new_data
            new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
    }
    
    cone = Volume2DX(header());
    cone.set_fourier(cut_data);
    
    missing_cone = Volume2DX(header());
    missing_cone.set_fourier(new_data);
}

void ds::Volume2DX::generate_fourier_noise(double fraction_to_change)
{   
    std::cout << "Randomly changing  " << fraction_to_change << " fraction of phases in Fourier space.. \n";
    
    ds::ReflectionData data = get_fourier();
    ds::ReflectionData new_data;
    int changes = 0;
    for(ReflectionData::const_iterator itr=data.begin(); itr!=data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        
        double rand_number = ((double) rand() / (RAND_MAX));
        if(rand_number < fraction_to_change)
        {
            double new_phase = ((double) rand() / (RAND_MAX)) * M_PI;
            Complex new_value = spot.value();
            new_value.set_phase(new_phase);
            new_data.set_spot_at(index.h(), index.k(), index.l(), new_value, spot.weight());
            changes++;
        }
        else
        {
            new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
    }
    
    std::cout << "Changed " << changes << " of " << data.spots() << "\n";
    set_fourier(new_data);
}

void ds::Volume2DX::generate_random_densities(double fraction_to_fill)
{
    ds::RealSpaceData data(nx(), ny(), nz());
    int attempts = fraction_to_fill * data.size();
    for ( int attempt = 0; attempt < attempts; attempt++ ) 
    {
        int id = rand() % data.size();
        double density = rand() % 255;
        data.set_value_at(id, density+data.get_value_at(id));
    }
    
    data.scale(0, 255);
    set_real(data);
}

void ds::Volume2DX::generate_poisson_densities(double mean_density)
{
    std::cout << "Generating Poisson noise with expected mean: " << mean_density << "\n";
    std::default_random_engine generator;
    std::poisson_distribution<int> distribution(mean_density);
    
    ds::RealSpaceData data(nx(), ny(), nz());
    for(int id=0; id<data.size(); id++)
    {
        data.set_value_at(id, distribution(generator));
    }
    
    data.grey_scale();
    
    set_real(data);
}

double ds::Volume2DX::resolution_at(int h, int k, int l) const
{
    ds::MillerIndex index(h, k, l);
    return tdx::utilities::fourier_utilities::get_resolution(index, _header.gamma(), _header.xlen(), _header.ylen(), _header.zlen());
}

ds::MillerIndex ds::Volume2DX::max_resolution_spot() const
{
    if(has_fourier())
    {
        MillerIndex spot;
        double maxres = 10000;
        for(ReflectionData::const_iterator itr=_fourier.begin(); itr!=_fourier.end(); ++itr)
        {
            MillerIndex index = (*itr).first;
            double res = resolution_at(index.h(), index.k(), index.l());
            if(res < maxres)
            {
                spot = index;
            }
        }
        return spot;
    }
    else
    {
        std::cerr << "WARNING: Can't calculate max resolution spot, Fourier data not in memory!!\n";
        return MillerIndex(0,0,0);
    }
}

double ds::Volume2DX::max_resolution() const
{
    MillerIndex index = max_resolution_spot();
    return resolution_at(index.h(), index.k(), index.l());
}

void ds::Volume2DX::rescale_energy(double energy)
{
    ReflectionData data = get_fourier();
    double curr_energy = data.intensity_sum();
    double factor = sqrt(energy/curr_energy);
    data.scale_amplitudes(factor);
    set_fourier(data);
}

void ds::Volume2DX::rescale_to_max_amplitude(double max_amplitude)
{
    ReflectionData data = get_fourier();
    double current_max = data.max_amplitude();
    double factor = max_amplitude/current_max;
    data.scale_amplitudes(factor);
    set_fourier(data);
}

void ds::Volume2DX::rescale_densities(double min, double max)
{
    RealSpaceData data = get_real();
    data.scale(min, max);
    this->set_real(data);
}

void ds::Volume2DX::grey_scale_densities()
{
    RealSpaceData data = get_real();
    data.grey_scale();
    this->set_real(data);
}

void ds::Volume2DX::symmetrize()
{
    std::cout << "Symmetrizing with symmetry: " << symmetry() << std::endl;
    tdx::symmetrization::Symmetry2dx sym(_header.symmetry());
    prepare_fourier();
    tdx::symmetrization::fourier_symmetrization::symmetrize(_fourier, sym);
    _type = FOURIER;
}

ds::BinnedData ds::Volume2DX::calculate_structure_factors(double min_freq, double max_freq, int resolution_bins)
{
    
    ds::BinnedData binned_data(min_freq, max_freq, resolution_bins);
    
    ReflectionData fourier_data = get_fourier();
    
    //Iterate over all possible h,k,l and populate intensity sums
    for(ReflectionData::const_iterator itr=fourier_data.begin(); itr!=fourier_data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        if ( index.h() != 0 || index.k() != 0 || index.l() != 0 ) 
        {
            double resolution = 1 / resolution_at(index.h(), index.k(), index.l());
            binned_data.add_data_at(resolution, pow(spot.value().amplitude(), 2));
        }
    }
    
    return binned_data;
}

ds::BinnedData ds::Volume2DX::fourier_shell_correlation(ds::Volume2DX reference, double min_freq, double max_freq, int resolution_bins)
{
    BinnedData binnedFSC(min_freq, max_freq, resolution_bins);
    BinnedData binned_numerator_sums(binnedFSC.min_range(), binnedFSC.max_range(), binnedFSC.bins());
    BinnedData binned_amp1_sums(binnedFSC.min_range(), binnedFSC.max_range(), binnedFSC.bins());
    BinnedData binned_amp2_sums(binnedFSC.min_range(), binnedFSC.max_range(), binnedFSC.bins());
    
    ReflectionData current_data = get_fourier();
    ReflectionData reference_data = reference.get_fourier();
    
    //Iterate over all reflections present in current Fourier space and consider
    //only the ones also present in reference volume
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        Complex value = (*itr).second.value();
        
        if(reference_data.exists(index.h(), index.k(), index.l()))
        {
            Complex ref_value = reference_data.value_at(index.h(), index.k(), index.l());
            Complex complex_numerator = value*ref_value.conjugate();
            
            double resolution = 1 / resolution_at(index.h(), index.k(), index.l());

            //Update the sums in the bins
            binned_amp1_sums.add_data_at(resolution, value.amplitude() * value.amplitude());
            binned_amp2_sums.add_data_at(resolution, ref_value.amplitude() * ref_value.amplitude());
            binned_numerator_sums.add_data_at(resolution, complex_numerator.real());
        }
    }
    
    //Calculate the FSC using the formula:
    // fsc = sum(real(f1*f2')/sqrt(sum(amp1^2)*sum(amp2^2)))
    for(int bin=0; bin<binnedFSC.bins(); bin++)
    {
        double denominator = sqrt(binned_amp1_sums.sum_in(bin)*binned_amp2_sums.sum_in(bin));
        if( denominator > 0.0000001)
        {
            double bin_fsc = binned_numerator_sums.sum_in(bin)/denominator;
            binnedFSC.set_bin_sum(bin, bin_fsc);
            binnedFSC.set_bin_count(bin, 1); //Just to get correct averages
        }
    }
    
    return binnedFSC;
}

ds::BinnedData ds::Volume2DX::fourier_conic_correlation(ds::Volume2DX reference, double min_cone_angle, double max_cone_angle, int bins)
{
    BinnedData binnedFCC(min_cone_angle, max_cone_angle, bins);
    BinnedData binned_numerator_sums(binnedFCC.min_range(), binnedFCC.max_range(), binnedFCC.bins());
    BinnedData binned_amp1_sums(binnedFCC.min_range(), binnedFCC.max_range(), binnedFCC.bins());
    BinnedData binned_amp2_sums(binnedFCC.min_range(), binnedFCC.max_range(), binnedFCC.bins());
    
    ReflectionData current_data = get_fourier();
    ReflectionData reference_data = reference.get_fourier();
    
    //Iterate over all reflections present in current Fourier space and consider
    //only the ones also present in reference volume
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        Complex value = (*itr).second.value();
        
        if(reference_data.exists(index.h(), index.k(), index.l()))
        {
            Complex ref_value = reference_data.value_at(index.h(), index.k(), index.l());
            Complex complex_numerator = value*ref_value.conjugate();
            
            double resolution = resolution_at(index.h(), index.k(), index.l());
            double val = std::abs(index.l()*1.0/nz())*resolution;
            double angle = 90 - acos(val)*180/M_PI;

            //Update the sums in the bins
            binned_amp1_sums.add_data_at(angle, value.amplitude() * value.amplitude());
            binned_amp2_sums.add_data_at(angle, ref_value.amplitude() * ref_value.amplitude());
            binned_numerator_sums.add_data_at(angle, complex_numerator.real());
        }
    }
    
    //Calculate the correlation using the formula:
    // sum(real(f1*f2')/sqrt(sum(amp1^2)*sum(amp2^2)))
    for(int bin=0; bin<binnedFCC.bins(); bin++)
    {
        double denominator = sqrt(binned_amp1_sums.sum_in(bin)*binned_amp2_sums.sum_in(bin));
        if( denominator > 0.0000001)
        {
            double bin_fsc = binned_numerator_sums.sum_in(bin)/denominator;
            binnedFCC.set_bin_sum(bin, bin_fsc);
            binnedFCC.set_bin_count(bin, 1); //Just to get correct averages
        }
    }
    
    return binnedFCC;
}

ds::MeshBinnedData ds::Volume2DX::fourier_conic_mesh_correlation(Volume2DX reference, double min_freq, double max_freq, double min_cone, double max_cone, int resolution_bins, int cone_bins)
{
    MeshBinnedData binnedFCMC(min_freq, max_freq, min_cone, max_cone, resolution_bins, cone_bins);
    MeshBinnedData binned_numerator_sums(min_freq, max_freq, min_cone, max_cone, resolution_bins, cone_bins);
    MeshBinnedData binned_amp1_sums(min_freq, max_freq, min_cone, max_cone, resolution_bins, cone_bins);
    MeshBinnedData binned_amp2_sums(min_freq, max_freq, min_cone, max_cone, resolution_bins, cone_bins);
    
    ReflectionData current_data = get_fourier();
    ReflectionData reference_data = reference.get_fourier();
    
    //Iterate over all reflections present in current Fourier space and consider
    //only the ones also present in reference volume
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        Complex value = (*itr).second.value();
        
        if(reference_data.exists(index.h(), index.k(), index.l()))
        {
            Complex ref_value = reference_data.value_at(index.h(), index.k(), index.l());
            double dot_prod = value.real()*ref_value.real() + value.imag()*ref_value.imag();
            
            double resolution = 1 / resolution_at(index.h(), index.k(), index.l());
            double angle = 90 - acos(std::abs(index.l())/sqrt(index.h()*index.h() + index.k()*index.k() + index.l()*index.l()))*180/M_PI;

            //Update the sums in the bins
            binned_amp1_sums.add_data_at(resolution, angle, value.amplitude() * value.amplitude());
            binned_amp2_sums.add_data_at(resolution, angle, ref_value.amplitude() * ref_value.amplitude());
            binned_numerator_sums.add_data_at(resolution, angle, dot_prod);
        }
    }
    
    //Calculate the correlation using the formula:
    // sum(real(f1*f2')/sqrt(sum(amp1^2)*sum(amp2^2)))
    for(int bin_x=0; bin_x<binnedFCMC.bins_x(); bin_x++)
    {
        for(int bin_y=0; bin_y<binnedFCMC.bins_y(); bin_y++)
        {
            double denominator = sqrt(binned_amp1_sums.sum_in(bin_x, bin_y)*binned_amp2_sums.sum_in(bin_x, bin_y));
            if( denominator > 0.0000001)
            {
                double bin_fsc = binned_numerator_sums.sum_in(bin_x, bin_y)/denominator;
                binnedFCMC.set_bin_sum(bin_x, bin_y, bin_fsc);
                binnedFCMC.set_bin_count(bin_x, bin_y, 1); //Just to get correct averages
            }
        }
    }
    
    return binnedFCMC;
}

void ds::Volume2DX::apply_structure_factors(ds::BinnedData sf_ref, double fraction)
{
    std::cout << "Applying structure factors to the volume.. \n";
    
    ReflectionData new_data;
    ReflectionData current_data = get_fourier();
    
    //Get the current structure factors with same resolution spacing as of reference
    ds::BinnedData sf_current = calculate_structure_factors(sf_ref.min_range(), sf_ref.max_range(), sf_ref.bins());
    
    double sf_ref_max = sf_ref.max_averaged_value();
    double sf_current_max = sf_current.max_averaged_value();
    double max_scale = sf_current_max/sf_ref_max;
    
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        
        if ( index.h() != 0 || index.k() != 0 || index.l() != 0 ) 
        {
            double resolution = 1 / resolution_at(index.h(), index.k(), index.l());

            //Find the appropriate intensity

            double sf_ref_intensity = sf_ref.average_at(resolution);
            double sf_curr_intensity = sf_current.average_at(resolution);

            if ( sf_ref_intensity != -1 && sf_curr_intensity != -1 ) {
                double amplitude_scale = 0.0;
                if ( sf_curr_intensity != 0.0 ) amplitude_scale = sqrt(max_scale * sf_ref_intensity / sf_curr_intensity);

                double current_amplitude = spot.amplitude();
                double new_amplitude = amplitude_scale*current_amplitude;

                double scaled_amplitude = new_amplitude * fraction + current_amplitude * (1 - fraction);

                //Insert the new value
                Complex new_value(spot.value());
                new_value.set_amplitude(scaled_amplitude);
                new_data.set_spot_at(index.h(), index.k(), index.l(), new_value, spot.weight());
            }
        }
    }
    
    set_fourier(new_data);
}


void ds::Volume2DX::write_bead_model_pdb(int no_of_beads, double density_threshold, double noise_level, std::string pdb_file)
{
    tdx::utilities::BeadModelGenerator generator(no_of_beads, density_threshold, noise_level);
    generator.generate_bead_model_coordinates(*this, pdb_file);
}

ds::Volume2DX ds::Volume2DX::generate_bead_model(int no_of_beads, double density_threshold, double bead_model_resolution)
{
    ds::Volume2DX bead_model(header());
    tdx::utilities::BeadModelGenerator generator(no_of_beads, density_threshold, 1.0, bead_model_resolution);
    
    ds::RealSpaceData bead_model_real_data = generator.generate_bead_model_volume(*this);
    bead_model.set_real(bead_model_real_data);
    
    return bead_model;
}


void ds::Volume2DX::invert_hand(int direction)
{
    ReflectionData data = get_fourier();
    ReflectionData new_data = data.inverted_data(direction);
    set_fourier(new_data);
}

void ds::Volume2DX::centerize_density_along_z()
{
    std::cout <<"Centering the density..\n";
    ReflectionData data = get_fourier();
    ReflectionData new_data;
    for(ReflectionData::const_iterator itr=data.begin(); itr!=data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        
        Complex new_value = spot.value();
        new_value.set_phase(spot.phase()+M_PI*index.l());
        
        new_data.set_spot_at(index.h(), index.k(), index.l(), new_value, spot.weight());
    }
    
    set_fourier(new_data);
    
}



void ds::Volume2DX::centerize_density_along_xyz()
{
    std::cout <<"Centering the density in x,y,z..\n";
    ReflectionData data = get_fourier();
    ReflectionData new_data;
    for(ReflectionData::const_iterator itr=data.begin(); itr!=data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        
        Complex new_value = spot.value();
        new_value.set_phase(spot.phase()+M_PI*index.h()+M_PI*index.k()+M_PI*index.l());
        
        new_data.set_spot_at(index.h(), index.k(), index.l(), new_value, spot.weight());
    }
    
    set_fourier(new_data);
    
}


void ds::Volume2DX::apply_density_histogram(Volume2DX reference)
{
    apply_density_histogram(reference, 1.0);
}

void ds::Volume2DX::apply_density_histogram(Volume2DX reference, double fraction)
{
    std::cout << "Applying density histogram\n";
    //Check the fraction
    if(fraction < 0.0 || fraction > 1.0)
    {
        std::cerr << "ERROR! The density histogram fraction can only be between 0 and 1";
        return;
    }
    
    RealSpaceData ref_real = RealSpaceData(reference.get_real());
    this->prepare_real();
    
    //Check the size of reference
    if(ref_real.size() != _real.size())
    {
        std::cerr << "ERROR! The size of reference volume " << ref_real.size() 
                  << " does not match to this volume's size " << _real.size()
                  << std::endl;
        return;
    }
    
    
    double* sorted_ref_values = ref_real.density_sorted_values();
    int* sorted_ids = _real.density_sorted_ids();
    
    RealSpaceData new_data(nx(), ny(), nz());
    for(int id=0; id<new_data.size(); id++)
    {
        int sorted_id = sorted_ids[id];
        double old_density = _real.get_value_at(sorted_id);
        double new_density = sorted_ref_values[id]*fraction + old_density*(1-fraction);
        new_data.set_value_at(sorted_id, new_density);
    }
    
    //new_data->scale(new_data->min(), _real->max());
    this->set_real(new_data);
    
}

void ds::Volume2DX::apply_density_threshold(double limit, double fraction)
{
   RealSpaceData data = get_real();
   data.threshold(limit, fraction);
   set_real(data);
}

void ds::Volume2DX::apply_real_mask(const RealSpaceData& mask, double fraction)
{
    RealSpaceData new_data = get_real();
    new_data.apply_mask(mask, fraction);   
    set_real(new_data);   
}

void ds::Volume2DX::apply_density_slab(double height, double fraction, bool centered)
{
    std::cout << "Applying density slab along vertical direction to the volume.. \n";
    RealSpaceData new_data = get_real();
    new_data.vertical_slab(height, fraction, centered);
    this->set_real(new_data);
}

void ds::Volume2DX::band_pass(double low_resolution, double high_resolution)
{
    if(low_resolution <= 0) low_resolution = resolution_at(0, 0, 0);
    if(high_resolution <= 0 ) high_resolution = 0.0;
    
    std::cout << "Band passing the resolution in range (" << low_resolution << ", " << high_resolution << ")\n";
    if(low_resolution <= high_resolution)
    {
        std::cerr << "ERROR: Cannot apply band pass filter with low_resolution > high_resolution!!";
        return;
    }
    
    ds::ReflectionData current_data = get_fourier();
    ReflectionData new_data;
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        double resolution = resolution_at(index.h(), index.k(), index.l());
        if(resolution >= high_resolution && resolution <= low_resolution)
        {
            new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
        
    }
    
    set_fourier(new_data);
    
}

void ds::Volume2DX::replace_reflections(const ReflectionData& fourier_data, double cone_angle, double replacement_amplitude_cutoff)
{
    ReflectionData current = get_fourier();
    current.replace_reflections(fourier_data, cone_angle, replacement_amplitude_cutoff);
    set_fourier(current);
}

void ds::Volume2DX::change_amplitudes(const ReflectionData& fourier_data)
{
    ReflectionData current = get_fourier();
    current.change_amplitudes(fourier_data, 0.00001);
    set_fourier(current);
}

void ds::Volume2DX::low_pass(double high_resolution)
{
    prepare_fourier();
    std::cout << "Current maximum resolution = " << max_resolution() << " A\n";
    band_pass(-1, high_resolution);
    std::cout << "Current maximum resolution = " << max_resolution() << " A\n";
}

void ds::Volume2DX::low_pass_butterworth(double high_resolution)
{
    prepare_fourier();
    std::cout << "Current maximum resolution = " << max_resolution() << " A\n";
    
    //double low_resolution = std::max(std::max(nx(), ny()), nz());
    //double low_resolution = nx()*ny()*nz();
    //double omegaL = 1.0/low_resolution;
    double omegaH = 1.0/high_resolution;
    //double eps = 0.882;
    //double aa = 10.624;
    //double order = 2.0f*log10(eps/sqrt(aa*aa-1.0f))/log10(omegaL/omegaH);
    //omegaL = omegaL/pow(eps,2.0f/order); 
    double order = 16;
    
    std::cout << "Low passing using Butterworth filter (order = " << order << ") with expected maximum resolution: " << high_resolution << " A\n";
    
    ds::ReflectionData current_data = get_fourier();
    ReflectionData new_data;
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        double resolution = 1/resolution_at(index.h(), index.k(), index.l());
        double weight = sqrt(1.0/(1.0+pow(resolution/omegaH, order)));
        new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value()*weight, spot.weight());
        //std::cout << index.to_string() << "(" << 1/resolution << " A)" << " had weight of: " << weight << "\n";
        
    }
    
    set_fourier(new_data);
    
    std::cout << "Current maximum resolution = " << max_resolution() << " A\n";
    
}

void ds::Volume2DX::low_pass_gaussian(double high_resolution)
{
    prepare_fourier();
    std::cout << "Current maximum resolution = " << max_resolution() << " A\n";
    
    std::cout << "Gaussian low pass with expected highest resolution = " << high_resolution << " A\n";
    
    double omega_square = 4*high_resolution*high_resolution;
    
    ds::ReflectionData current_data = get_fourier();
    ReflectionData new_data;
    for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        PeakData spot = (*itr).second;
        double resolution = 1/resolution_at(index.h(), index.k(), index.l());
        double weight = exp(-1*resolution*resolution*omega_square);
        new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value()*weight, spot.weight());
        //std::cout << index.to_string() << "(" << 1/resolution << " A)" << " had weight of: " << weight << "\n";
        
    }
    
    set_fourier(new_data);
    
    std::cout << "Current maximum resolution = " << max_resolution() << " A\n";
    
}

ds::Volume2DX ds::Volume2DX::project2D(char axis)
{
    VolumeHeader head = header();
    ReflectionData current_data = get_fourier();
    ReflectionData new_data;
    if(axis == 'x' || axis == 'X')
    {
        head.set_mx(1);
        head.set_rows(1);
        for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
        {
            //Get the data for current reflection
            MillerIndex index = (*itr).first;
            PeakData spot = (*itr).second;
            if(index.h() == 0) new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
    }
    else if(axis == 'y' || axis == 'Y')
    {
        head.set_my(1);
        head.set_columns(1);
        for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
        {
            //Get the data for current reflection
            MillerIndex index = (*itr).first;
            PeakData spot = (*itr).second;
            if(index.k() == 0) new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
    }
    else if(axis == 'z' || axis == 'Z')
    {
        head.set_mz(1);
        head.set_sections(1);
        for(ReflectionData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
        {
            //Get the data for current reflection
            MillerIndex index = (*itr).first;
            PeakData spot = (*itr).second;
            if(index.l() == 0) new_data.set_spot_at(index.h(), index.k(), index.l(), spot.value(), spot.weight());
        }
    }
    else
    {
        std::cerr << "ERROR: Bad value provided for axis: " << axis << " should be (x ,or, y ,or, z)\n";
        exit(1);
    }
    
    Volume2DX projection(head);
    projection.set_fourier(new_data);
    
    return projection;
}

ds::Volume2DX ds::Volume2DX::subsample(int factor)
{
    std::cout << "Sub-sampling volume by " << factor << " times.\n";
    
    int new_nx = factor*nx();
    int new_ny = factor*ny();
    int new_nz = factor*nz();
    
    VolumeHeader head = header();
    head.set_mx(new_nx);
    head.set_my(new_ny);
    head.set_mz(new_nz);
    Volume2DX new_volume(head);
    
    RealSpaceData data = get_real();
    RealSpaceData new_data(new_nx, new_ny, new_nz);
    for(int ix=0; ix<new_nx; ix++)
    {
        for(int iy=0; iy<new_ny; iy++)
        {
            for(int iz=0; iz<new_nz; iz++)
            {   
                int data_x = (int) ix/factor;
                int data_y = (int) iy/factor;
                int data_z = (int) iz/factor;
                new_data.set_value_at(ix, iy, iz, data.get_value_at(data_x, data_y, data_z));
            }
        }
    }
    
    new_volume.set_real(new_data);
    
    return new_volume;
}

ds::Volume2DX ds::Volume2DX::extended_volume(int x_cells, int y_cells, int z_cells)
{
    std::cout << "Extending volume to " << x_cells+1 << " X " << y_cells+1 << " X " << z_cells+1 << " unit cells \n";
    
    int new_nx = (x_cells+1)*nx();
    int new_ny = (y_cells+1)*ny();
    int new_nz = (z_cells+1)*nz();
    
    VolumeHeader new_header = header();
    new_header.reset_size(new_nx, new_ny, new_nz);
    Volume2DX new_volume(new_header);

    RealSpaceData data = get_real();
    RealSpaceData new_data(new_nx, new_ny, new_nz);
    for(int ix=0; ix<new_nx; ix++)
    {
        for(int iy=0; iy<new_ny; iy++)
        {
            for(int iz=0; iz<new_nz; iz++)
            {   
                int data_x = ix % nx();
                int data_y = iy % ny();
                int data_z = iz % nz();
                new_data.set_value_at(ix, iy, iz, data.get_value_at(data_x, data_y, data_z));
            }
        }
    }
    
    new_volume.set_real(new_data);
    
    return new_volume;
}

ds::Volume2DX ds::Volume2DX::zero_phases()
{
    std::cout << "Zeroing phases in volume \n";
    
    Volume2DX new_volume(header());
    
    ReflectionData data = get_fourier();
    ReflectionData new_data;
    for(ReflectionData::const_iterator itr=data.begin(); itr!=data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        Complex value = (*itr).second.value();
        value.set_phase(0);
        new_data.set_spot_at(index.h(), index.k(), index.l(), value, (*itr).second.weight());
    }
    
    new_volume.set_fourier(new_data);
    
    return new_volume;
}

ds::Volume2DX ds::Volume2DX::spread_fourier_data()
{
    ReflectionData data = get_fourier();
    data.spread_data();
    
    Volume2DX new_volume(header());
    new_volume.set_fourier(data);
    return new_volume;
}

void ds::Volume2DX::extend_to_full_fourier() 
{
    ReflectionData data = get_fourier();
    ReflectionData new_data = data.get_full_fourier();
    set_fourier(new_data);
}

ds::Volume2DX ds::Volume2DX::apply_bfactor(double negative_temp_factor)
{
    std::cout << "Applying a negative B-factor of: " << negative_temp_factor << "\n";
    
    Volume2DX new_volume(header());
    
    ReflectionData data = get_fourier();
    ReflectionData new_data;
    for(ReflectionData::const_iterator itr=data.begin(); itr!=data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        Complex value = (*itr).second.value();
        double resolution = resolution_at(index.h(), index.k(), index.l());
        double weight = exp(-1*negative_temp_factor/(4*resolution*resolution));
        new_data.set_spot_at(index.h(), index.k(), index.l(), value*weight, (*itr).second.weight());
    }
    
    new_volume.set_fourier(new_data);
    
    return new_volume;
}

double ds::Volume2DX::density_at(int x, int y, int z)
{
    return get_real().get_value_at(x,y,z);
}

int ds::Volume2DX::nx() const
{
    return _header.rows();   
}

int ds::Volume2DX::ny() const
{
    return _header.columns();
}

int ds::Volume2DX::nz() const
{
    return _header.sections();
}

int ds::Volume2DX::fx() const{
    return (int) (nx()/2+1);
}

int ds::Volume2DX::fy() const{
    return ny();
}

int ds::Volume2DX::fz() const{
    return nz();
}


int ds::Volume2DX::h_max() const{
    return fx()-1;
}

int ds::Volume2DX::k_max() const{
    return (int) fy()/2;
}

int ds::Volume2DX::l_max() const{
    return (int) fz()/2;
}

double ds::Volume2DX::xlen() const {
    return _header.xlen();
}

void ds::Volume2DX::set_xlen(double xlen)
{
    _header.set_xlen(xlen);
}

double ds::Volume2DX::ylen() const {
    return _header.ylen();
}

void ds::Volume2DX::set_ylen(double ylen)
{
    _header.set_ylen(ylen);
}

double ds::Volume2DX::zlen() const {
    return _header.zlen();
}

void ds::Volume2DX::set_zlen(double zlen)
{
    _header.set_zlen(zlen);
}

double ds::Volume2DX::gamma() const
{
    return _header.gamma();
}

void ds::Volume2DX::set_gamma_radians(double gamma)
{
    _header.set_gamma(gamma);
}

void ds::Volume2DX::set_gamma_degrees(double gamma)
{
    _header.set_gamma(tdx::utilities::angle_utilities::DegreeToRadian(gamma));
}

std::string ds::Volume2DX::symmetry() const
{
    return _header.symmetry();
}

void ds::Volume2DX::set_symmetry(std::string symmetry)
{
    _header.set_symmetry(symmetry);
}
