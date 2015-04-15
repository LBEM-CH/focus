
#include "volume_header.hpp"

#include "../utilities/angle_utilities.hpp"

namespace ds = volume_processing_2dx::data_structures;

ds::VolumeHeader2dx::VolumeHeader2dx()
{
    this->initialize(0, 0, 0);
}

ds::VolumeHeader2dx::VolumeHeader2dx(int nx, int ny, int nz)
{
    this->initialize(nx, ny, nz);
}

void ds::VolumeHeader2dx::initialize(int nx, int ny, int nz)
{
    _nx = nx;
    _ny = ny;
    _nz = nz;
    _xlen = (double) nx;
    _ylen = (double) ny;
    _zlen = (double) nz;
    set_gamma(volume_processing_2dx::utilities::angle_utilities::DegreeToRadian(90));
    set_symmetry("P1");
    set_max_resolution(2.0);
    set_membrane_height(1.0);

}

void ds::VolumeHeader2dx::reset_size(int nx, int ny, int nz)
{
    _nx = nx;
    _ny = ny;
    _nz = nz;
    if((int)_xlen == _nx) _xlen = (double) nx;
    if((int)_ylen == _ny) _ylen = (double) ny;
    if((int)_zlen == _nz) _zlen = (double) nz;
} 

void ds::VolumeHeader2dx::set_nx(int nx)
{
    this->_nx = nx;
}

void ds::VolumeHeader2dx::set_ny(int ny)
{
    this->_ny = ny;
}

void ds::VolumeHeader2dx::set_nz(int nz)
{
    this->_nz = nz;
}

void ds::VolumeHeader2dx::set_xlen(double xlen)
{
    this->_xlen = xlen;
}

void ds::VolumeHeader2dx::set_ylen(double ylen)
{
    this->_ylen = ylen;
}

void ds::VolumeHeader2dx::set_zlen(double zlen)
{
    this->_zlen = zlen;
}

void ds::VolumeHeader2dx::set_symmetry(std::string symmetry)
{
    _symmetry = volume_processing_2dx::symmetrization::Symmetry2dx(symmetry);
}

void ds::VolumeHeader2dx::set_gamma(double gamma)
{
    _gamma = gamma;
}

void ds::VolumeHeader2dx::set_max_resolution(double resolution)
{
    _max_resolution = resolution;
}

void ds::VolumeHeader2dx::set_membrane_height(double membrane_height)
{
    if(membrane_height > 0.0 && membrane_height <= 1.0)
    {
        _membrane_height = membrane_height;
    }
    else
    {
        throw new std::invalid_argument("Membrane height expected in fractions, found: " + std::to_string(membrane_height));
    }
}

int ds::VolumeHeader2dx::nx() const
{
    return _nx;
}

int ds::VolumeHeader2dx::ny() const
{
    return _ny;
}

int ds::VolumeHeader2dx::nz() const
{
    return _nz;
}

double ds::VolumeHeader2dx::xlen() const
{
    return _xlen;
}

double ds::VolumeHeader2dx::ylen() const
{
    return _ylen;
}

double ds::VolumeHeader2dx::zlen() const
{
    return _zlen;
}

double ds::VolumeHeader2dx::gamma() const
{
    return _gamma;
}

std::string ds::VolumeHeader2dx::symmetry() const
{
    return _symmetry.symmetry_string();
}

int ds::VolumeHeader2dx::symmetry_2dx_code() const
{
    return _symmetry.symmetry_code();
}

int ds::VolumeHeader2dx::symmetry_ccp4_code() const
{
    return _symmetry.ccp4_index();
}

double ds::VolumeHeader2dx::max_resolution() const
{
    return _max_resolution;
}

double ds::VolumeHeader2dx::membrane_height() const
{
    return _membrane_height;
}
