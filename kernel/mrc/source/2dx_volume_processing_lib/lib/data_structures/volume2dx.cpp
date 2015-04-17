/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "volume2dx.hpp"

#include "miller_index.hpp"
#include "../io/mrc_reader.hpp"
#include "../io/hkz_reader.hpp"
#include "../io/mrc_writer.hpp"
#include "../io/hkl_writer.hpp"

#include "../symmetrization/symmetry2dx.hpp"
#include "../symmetrization/fourier_symmetrization.hpp"

#include "../utilities/filesystem.hpp"
#include "../utilities/fourier_utilities.hpp"
#include "../utilities/bead_model_generator.hpp"

namespace ds = volume::data;

ds::Volume2dx::Volume2dx()
{
    std::cout << "Initializing volume.. ";
    this->initialize(0, 0, 0);
    std::cout << "Done.\n";
}

ds::Volume2dx::Volume2dx(int nx, int ny, int nz)
{
    std::cout << "Initializing volume.. ";
    this->initialize(nx, ny, nz);
    std::cout << "Done.\n";
    
}

void ds::Volume2dx::initialize(int nx, int ny, int nz)
{
    std::cout << "Creating blank volume with size (" << nx << ", " << ny << ", " << nz << ") \n";
    _header = ds::VolumeHeader2dx(nx, ny, nz);
    _real = ds::RealSpaceData(nx, ny, nz);
    _fourier = ds::FourierSpaceData();
    _transform = volume::transforms::FourierTransformFFTW();
    _type = NONE;
}

void ds::Volume2dx::reset(int nx, int ny, int nz)
{
    std::cout << "Reseting the volume with size (" << nx << ", " << ny << ", " << nz << ") \n";
    std::cout << "NOTE: The data of the volume is also cleared!!!!\n";
    _header.reset_size(nx, ny, nz);
    _real.reset(nx, ny, nz);
    _fourier.reset();
    _type = NONE;
}

void ds::Volume2dx::read_volume(std::string file_name, std::string format)
{
    std::cout << "Reading volume with format: "<< format << std::endl;
    if(format == "hkz")
    {
       _fourier = volume::io::hkz_reader::read(file_name, _header);
       _type = FOURIER;
    }
    else if (format == "mrc")
    {
        if(volume::io::MrcReader(file_name).mrc_to_real(_header, _real))
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
    std::cout << "Writing volume with format: "<< format << std::endl;
    if(format == "hkl")
    {
        volume::io::hkl_writer::write(file_name, get_fourier());
    }
    else if (format == "mrc")
    {
        volume::io::mrc_writer::write_real(file_name, _header, get_real());
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
    return volume::utilities::fourier_utilities::GetResolution(index, _header.gamma(), _header.xlen(), _header.ylen(), _header.zlen());
}

void ds::Volume2dx::symmetrize()
{
    std::cout << "Symmetrizing with symmetry: " << symmetry() << std::endl;
    volume::symmetrization::Symmetry2dx sym(_header.symmetry());
    volume::symmetrization::fourier_symmetrization::symmetrize(_fourier, sym);
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
    if(!has_real()) real_from_fourier();
    return _real;
}

ds::FourierSpaceData ds::Volume2dx::get_fourier()
{
    if(!has_fourier()) fourier_from_real();
    return _fourier;
}

void ds::Volume2dx::set_fourier(FourierSpaceData& fourier)
{
    this->_fourier = fourier;
    _type = FOURIER;
}

void ds::Volume2dx::set_real(RealSpaceData& real)
{
    this->_real = real;
    _type = REAL;
}

void ds::Volume2dx::fourier_from_real()
{
    std::cout << "Converting the real data to Fourier data.. \n";
    if(_type == REAL){
        _fourier.reset();
        fftw_complex* complex_data = (fftw_complex*) malloc(fx()*fy()*fz()*sizeof(fftw_complex));
        std::cout << "Calling FFTW libraries..\n";
        _transform.RealToComplex(nx(), ny(), nz(), _real.get_data(), complex_data);
        std::cout << "Done.\n";
        _fourier.reset_data_from_fftw(fx(), fy(), fz(), complex_data);
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
        std::cout << "Calling FFTW libraries..\n";
        _transform.ComplexToReal(nx(), ny(), nz(), _fourier.fftw_data(fx(), fy(), fz()), real_data);
        std::cout << "Done.\n";
        _type = BOTH;
        _real.reset_data(real_data);
    }
    else if(_type == NONE) {
        std::cerr << "Hey, Real data cannot be set! Fourier data not in memory. Did you forget to set the data?";
    }
}

void ds::Volume2dx::write_bead_model_pdb(int no_of_beads, double density_threshold, double noise_level, std::string pdb_file)
{
    volume::utilities::BeadModelGenerator generator(no_of_beads, density_threshold, noise_level);
    generator.generate_bead_model_coordinates(*this, pdb_file);
}

void ds::Volume2dx::apply_density_histogram(Volume2dx reference)
{
    apply_density_histogram(reference, 1.0);
}

void ds::Volume2dx::apply_density_histogram(Volume2dx reference, double fraction)
{
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
    return _header.nx();   
}

int ds::Volume2dx::ny() const
{
    return _header.ny();
}

int ds::Volume2dx::nz() const
{
    return _header.nz();
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
    return _header.xlen();
}

void ds::Volume2dx::set_xlen(double xlen)
{
    _header.set_xlen(xlen);
}

double ds::Volume2dx::ylen() const {
    return _header.ylen();
}

void ds::Volume2dx::set_ylen(double ylen)
{
    _header.set_ylen(ylen);
}

double ds::Volume2dx::zlen() const {
    return _header.zlen();
}

void ds::Volume2dx::set_zlen(double zlen)
{
    _header.set_zlen(zlen);
}

double ds::Volume2dx::gamma() const
{
    return _header.gamma();
}

void ds::Volume2dx::set_gamma(double gamma)
{
    _header.set_gamma(gamma);
}

std::string ds::Volume2dx::symmetry() const
{
    return _header.symmetry();
}

void ds::Volume2dx::set_symmetry(std::string symmetry)
{
    _header.set_symmetry(symmetry);
}

double ds::Volume2dx::max_resolution() const {
    return _header.max_resolution();
}

void ds::Volume2dx::set_max_resolution(double resolution)
{
    _header.set_max_resolution(resolution);
}
