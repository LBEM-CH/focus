/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <math.h>

#include "volume2dx.hpp"

#include "miller_index.hpp"
#include "diffraction_spot.hpp"

#include "../io/mrc_reader.hpp"
#include "../io/hkz_reader.hpp"
#include "../io/hkl_reader.hpp"
#include "../io/mrc_writer.hpp"
#include "../io/hkl_writer.hpp"

#include "../symmetrization/symmetry2dx.hpp"
#include "../symmetrization/fourier_symmetrization.hpp"

#include "../utilities/filesystem.hpp"
#include "../utilities/fourier_utilities.hpp"
#include "../utilities/bead_model_generator.hpp"
#include "../utilities/number_utilities.hpp"

namespace ds = volume::data;

ds::Volume2dx::Volume2dx(int nx, int ny, int nz)
{
    _header = new ds::VolumeHeader2dx(nx, ny, nz);
    _real = new ds::RealSpaceData(nx, ny, nz);
    _fourier = new ds::FourierSpaceData();
    _transform = new volume::transforms::FourierTransformFFTW();
    _type = NONE;
}

void ds::Volume2dx::reset(int nx, int ny, int nz)
{
    std::cout << "Reseting the volume with size (" << nx << ", " << ny << ", " << nz << ") \n";
    std::cout << "NOTE: The data of the volume is also cleared!!!!\n";
    _header->reset_size(nx, ny, nz);
    _real->reset(nx, ny, nz);
    _fourier->reset();
    _type = NONE;
}

std::string ds::Volume2dx::to_string() const
{
    std::string output = "";
    
    output += _header->to_string();
    
    output += "\nData Information:\n";
    if(has_real())
    {
        output += "\tReal data in memory.\n" ;
        output += "\t|Minimum density: " + std::to_string(_real->min()) + "\n";
        output += "\t|Maximum density: " + std::to_string(_real->max()) + "\n";
        output += "\t|Mean density: " + std::to_string(_real->mean()) + "\n";
        output += "\n";
    }
    
    if(has_fourier())
    {
        output += "\tFourier data in memory.\n" ;
        output += "\t|Number of spots: " + std::to_string(_fourier->spots()) + "\n";
        output += "\t|Intensity sum: " + std::to_string(_fourier->intensity_sum()) + "\n";
        output += "\n";
    }
    
    if(_type == NONE)
    {
        output += "\tNo data in memory\n";
        output += "\n";
    }
    
    return output;
    
}

void ds::Volume2dx::read_volume(std::string file_name, std::string format)
{
    std::cout << "Reading volume with format <"<< format << "> from file:\n\t" << file_name << "\n\n";
    if(format == "hkl")
    {
       *_fourier = volume::io::hkl_reader::read(file_name, *_header);
       _type = FOURIER;
    }
    if(format == "hkz")
    {
       *_fourier = volume::io::hkz_reader::read(file_name, *_header);
       _type = FOURIER;
    }
    else if (format == "mrc")
    {
        if(volume::io::MrcReader(file_name).mrc_to_real(*_header, *_real))
        {
            _type = REAL;
        }
    }
    else
    {
        std::cerr << "The read format <" << format << "> of file " << file_name << " not supported.\n";
    }
    
    std::cout << "Volume in memory!\n";
    
}

void ds::Volume2dx::read_volume(std::string file_name)
{
    std::string format = volume::utilities::filesystem::FileExtension(file_name);
    
    read_volume(file_name, format);
}

void ds::Volume2dx::write_volume(std::string file_name, std::string format)
{
    std::cout << "Writing volume with format <"<< format << "> to file:\n\t" << file_name << "\n\n";
    if(format == "hkl")
    {
        volume::io::hkl_writer::write(file_name, get_fourier());
    }
    else if (format == "mrc")
    {
        volume::io::mrc_writer::write_real(file_name, *_header, get_real());
    }
    else
    {
        std::cerr << "The write format <" << format << "> of file " << file_name << " not supported.\n";
    }
}

void ds::Volume2dx::write_volume(std::string file_name)
{
    std::string format = volume::utilities::filesystem::FileExtension(file_name);
    write_volume(file_name, format);
}

double ds::Volume2dx::resolution_at(int h, int k, int l) const
{
    ds::MillerIndex index(h, k, l);
    return volume::utilities::fourier_utilities::GetResolution(index, _header->gamma(), _header->xlen(), _header->ylen(), _header->zlen());
}

void ds::Volume2dx::symmetrize()
{
    std::cout << "Symmetrizing with symmetry: " << symmetry() << std::endl;
    volume::symmetrization::Symmetry2dx sym(_header->symmetry());
    volume::symmetrization::fourier_symmetrization::symmetrize(*_fourier, sym);
}

bool ds::Volume2dx::has_fourier() const
{
    if(_type == FOURIER || _type == BOTH) return true;
    else return false;
}

bool ds::Volume2dx::has_real() const
{
    if(_type == REAL || _type == BOTH) return true;
    else return false;
}

ds::RealSpaceData ds::Volume2dx::get_real()
{
    prepare_real();
    return *_real;
}

ds::FourierSpaceData ds::Volume2dx::get_fourier()
{
    prepare_fourier();
    return *_fourier;
}

void ds::Volume2dx::prepare_fourier()
{
    if(!has_fourier()) fourier_from_real();
}

void ds::Volume2dx::prepare_real()
{
    if(!has_real()) real_from_fourier();
}


void ds::Volume2dx::set_fourier(FourierSpaceData& fourier)
{
    *_fourier = fourier;
    _type = FOURIER;
}

void ds::Volume2dx::set_real(RealSpaceData& real)
{
    if(real.nx() == nx() && real.ny() == ny() && real.nz() == nz())
    {   
        *_real = real;
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

void ds::Volume2dx::fourier_from_real()
{
    std::cout << "Converting the real data to Fourier data.. \n";
    if(_type == REAL){
        _fourier->reset();
        fftw_complex* complex_data = (fftw_complex*) malloc(fx()*fy()*fz()*sizeof(fftw_complex));
        _transform->RealToComplex(nx(), ny(), nz(), _real->get_data(), complex_data);
        _fourier->reset_data_from_fftw(fx(), fy(), fz(), complex_data);
        _type = BOTH;
    }
    else if(_type == NONE) {
        std::cerr << "Hey, Fourier data cannot be set! Real data not in memory. Did you forget to set the data?";
    }
}

void ds::Volume2dx::real_from_fourier()
{
    std::cout << "Converting the Fourier data to real. \n";
    if(_type == FOURIER){
        double* real_data = (double*) malloc(nx()*ny()*nz()*sizeof(double));
        _transform->ComplexToReal(nx(), ny(), nz(), _fourier->fftw_data(fx(), fy(), fz()), real_data);
        _type = BOTH;
        _real->reset_data(real_data);
    }
    else if(_type == NONE) {
        std::cerr << "Hey, Real data cannot be set! Fourier data not in memory. Did you forget to set the data?";
    }
}

ds::StructureFactors ds::Volume2dx::calculate_structure_factors(int resolution_bins)
{
    double min_resolution = 0;
    double max_resolution = 1/this->max_resolution();
    
    std::cout << "Calculating structure factors in range(" << min_resolution 
              << ", " << max_resolution <<") with " << resolution_bins << " bins..\n";
    
    ds::StructureFactors sf(min_resolution, max_resolution, resolution_bins);
    sf.initialize_intensities(*this);
    
    return sf;
}

void ds::Volume2dx::apply_structure_factors(ds::StructureFactors sf_ref, double fraction)
{
    std::cout << "Applying structure factors to the volume.. \n";
    
    FourierSpaceData new_data;
    FourierSpaceData current_data = get_fourier();
    
    //Get the current structure factors with same resolution spacing as of reference
    ds::StructureFactors sf_current(sf_ref.min_resolution(), sf_ref.max_resolution(), sf_ref.resolution_bins());
    sf_current.initialize_intensities(*this);
    
    double sf_ref_max = sf_ref.max_intensity();
    double sf_current_max = sf_current.max_intensity();
    double max_scale = sf_current_max/sf_ref_max;
    
    for(FourierSpaceData::const_iterator itr=current_data.begin(); itr!=current_data.end(); ++itr)
    {
        //Get the data for current reflection
        MillerIndex index = (*itr).first;
        DiffractionSpot spot = (*itr).second;
        
        if ( index.h() != 0 || index.k() != 0 || index.l() != 0 ) 
        {
            double resolution = 1 / resolution_at(index.h(), index.k(), index.l());

            //Find the appropriate intensity

            double sf_ref_intensity = sf_ref.intensity_at(resolution);
            double sf_curr_intensity = sf_current.intensity_at(resolution);

            if ( sf_ref_intensity != -1 && sf_curr_intensity != -1 ) {
                double amplitude_scale = 0.0;
                if ( sf_curr_intensity != 0.0 ) amplitude_scale = sqrt(max_scale * sf_ref_intensity / sf_curr_intensity);

                double current_amplitude = spot.amplitude();
                double new_amplitude = amplitude_scale*current_amplitude;

                double scaled_amplitude = new_amplitude * fraction + current_amplitude * (1 - fraction);

                //Insert the new value
                Complex2dx new_value(spot.value());
                new_value.set_amplitude(scaled_amplitude);
                new_data.set_value_at(index.h(), index.k(), index.l(), new_value, spot.weight());
            }
        }
    }
    
    set_fourier(new_data);
}


void ds::Volume2dx::write_bead_model_pdb(int no_of_beads, double density_threshold, double noise_level, std::string pdb_file)
{
    volume::utilities::BeadModelGenerator generator(no_of_beads, density_threshold, noise_level);
    generator.generate_bead_model_coordinates(*this, pdb_file);
}

ds::Volume2dx ds::Volume2dx::generate_bead_model(int no_of_beads, double density_threshold, double noise_level)
{
    ds::Volume2dx bead_model(*this);
    volume::utilities::BeadModelGenerator generator(no_of_beads, density_threshold, noise_level);
    
    ds::RealSpaceData bead_model_real_data = generator.generate_bead_model_volume(*this);
    bead_model.set_real(bead_model_real_data);
    
    return bead_model;
}

void ds::Volume2dx::centerize_density_along_z()
{
    std::cout <<"Centering the density..\n";
    FourierSpaceData data = get_fourier();
    FourierSpaceData new_data;
    for(FourierSpaceData::const_iterator itr=data.begin(); itr!=data.end(); ++itr)
    {
        MillerIndex index = (*itr).first;
        DiffractionSpot spot = (*itr).second;
        
        Complex2dx new_value = spot.value();
        new_value.set_phase(spot.phase()+M_PI*index.l());
        
        new_data.set_value_at(index.h(), index.k(), index.l(), new_value, spot.weight());
    }
    
    set_fourier(new_data);
    
}


void ds::Volume2dx::apply_density_histogram(Volume2dx reference)
{
    apply_density_histogram(reference, 1.0);
}

void ds::Volume2dx::apply_density_histogram(Volume2dx reference, double fraction)
{
    std::cout << "\nApplying density histogram\n";
    //Check the fraction
    if(fraction < 0.0 || fraction > 1.0)
    {
        std::cerr << "ERROR! The density histogram fraction can only be between 0 and 1";
        return;
    }
    
    RealSpaceData ref_real = reference.get_real();
    RealSpaceData this_real = this->get_real();
    
    //Check the size of reference
    if(ref_real.size() != this_real.size())
    {
        std::cerr << "ERROR! The size of reference volume " << ref_real.size() 
                  << " does not match to this volume's size " << this_real.size()
                  << std::endl;
        return;
    }
    
    
    double* sorted_ref_values = ref_real.density_sorted_values();
    int* sorted_ids = this_real.density_sorted_ids();
    
    RealSpaceData new_data(nx(), ny(), nz());
    for(int id=0; id<this_real.size(); id++)
    {
        int sorted_id = sorted_ids[id];
        double old_density = this_real.get_value_at(sorted_id);
        double new_density = sorted_ref_values[id]*fraction + old_density*(1-fraction);
        new_data.set_value_at(sorted_id, new_density);
    }
    
    this->set_real(new_data);
    
}

double ds::Volume2dx::density_at(int x, int y, int z)
{
    return get_real().get_value_at(x,y,z);
}

int ds::Volume2dx::nx() const
{
    return _header->nx();   
}

int ds::Volume2dx::ny() const
{
    return _header->ny();
}

int ds::Volume2dx::nz() const
{
    return _header->nz();
}

int ds::Volume2dx::fx() const{
    return (int) (nx()/2+1);
}

int ds::Volume2dx::fy() const{
    return ny();
}

int ds::Volume2dx::fz() const{
    return nz();
}


int ds::Volume2dx::h_max() const{
    return fx()-1;
}

int ds::Volume2dx::k_max() const{
    return (int) fy()/2;
}

int ds::Volume2dx::l_max() const{
    return (int) fz()/2;
}

double ds::Volume2dx::xlen() const {
    return _header->xlen();
}

void ds::Volume2dx::set_xlen(double xlen)
{
    _header->set_xlen(xlen);
}

double ds::Volume2dx::ylen() const {
    return _header->ylen();
}

void ds::Volume2dx::set_ylen(double ylen)
{
    _header->set_ylen(ylen);
}

double ds::Volume2dx::zlen() const {
    return _header->zlen();
}

void ds::Volume2dx::set_zlen(double zlen)
{
    _header->set_zlen(zlen);
}

double ds::Volume2dx::gamma() const
{
    return _header->gamma();
}

void ds::Volume2dx::set_gamma(double gamma)
{
    _header->set_gamma(gamma);
}

std::string ds::Volume2dx::symmetry() const
{
    return _header->symmetry();
}

void ds::Volume2dx::set_symmetry(std::string symmetry)
{
    _header->set_symmetry(symmetry);
}

double ds::Volume2dx::max_resolution() const {
    return _header->max_resolution();
}

void ds::Volume2dx::set_max_resolution(double resolution)
{
    _header->set_max_resolution(resolution);
}
