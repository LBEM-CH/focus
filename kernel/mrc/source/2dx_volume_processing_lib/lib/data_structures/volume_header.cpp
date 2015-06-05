
#include "volume_header.hpp"

#include "../utilities/angle_utilities.hpp"

namespace ds = volume::data;

ds::VolumeHeader2dx::VolumeHeader2dx()
{
    initialize(0, 0, 0);
}

ds::VolumeHeader2dx::VolumeHeader2dx(int nx, int ny, int nz)
{
    initialize(nx, ny, nz);
}

void ds::VolumeHeader2dx::initialize(int nx, int ny, int nz)
{
    _rows = nx;
    _columns = ny;
    _sections = nz;
    _mx = nx;
    _my = ny;
    _mz = nz;
    _xlen = (double) nx;
    _ylen = (double) ny;
    _zlen = (double) nz;
    _nxstart = 0;
    _nystart = 0;
    _nzstart = 0;
    set_gamma(volume::utilities::angle_utilities::DegreeToRadian(90));
    set_symmetry("P1");
    set_max_resolution(2.0);

}

void ds::VolumeHeader2dx::reset_size(int nx, int ny, int nz)
{
    _rows = nx;
    _columns = ny;
    _sections = nz;
    if(_mx == 0) _mx = nx;
    if(_my == 0) _my = ny;
    if(_mz == 0) _mz = nz;
    if(_xlen == 0.0) _xlen = (double) nx;
    if(_ylen == 0.0) _ylen = (double) ny;
    if(_zlen == 0.0) _zlen = (double) nz;
} 

std::string ds::VolumeHeader2dx::to_string() const
{
    std::string output = "";
    double ninty = 90.0;
    output += "Volume Information:\n";
    output += "\t|Size (rows, columns, sections): " + std::to_string(rows()) + " X " + 
               std::to_string(columns()) + " X " + std::to_string(sections()) + "\n";
    output += "\t|Grid size (x, y, z): " + std::to_string(mx()) + " X " + 
               std::to_string(my()) + " X " + std::to_string(mz()) + "\n";
    output += "\t|Cell lengths: " + std::to_string(xlen()) + ", " + 
               std::to_string(ylen()) + ", " + std::to_string(zlen()) + "\n";
    output += "\t|Cell angles: " + std::to_string(ninty) + ", " + 
               std::to_string(ninty) + ", " + 
               std::to_string(volume::utilities::angle_utilities::RadianToDegree(gamma())) + "\n";
    output += "\t|Symmetry: " + symmetry() + "\n";
    output += "\t|Start indices: " + std::to_string(nxstart()) + " , " + 
               std::to_string(nystart()) + " , " + std::to_string(nzstart()) + "\n";
    output += "\t|Maximum Resolution: " + std::to_string(max_resolution()) + "\n";
    
    return output;
}

void ds::VolumeHeader2dx::set_rows(int rows)
{
    this->_rows = rows;
}

void ds::VolumeHeader2dx::set_columns(int columns)
{
    this->_columns = columns;
}

void ds::VolumeHeader2dx::set_sections(int sections)
{
    this->_sections = sections;
}

void ds::VolumeHeader2dx::set_mx(int mx)
{
    this->_mx = mx;
}

void ds::VolumeHeader2dx::set_my(int my)
{
    this->_my = my;
}

void ds::VolumeHeader2dx::set_mz(int mz)
{
    this->_mz = mz;
}

void ds::VolumeHeader2dx::set_nxstart(int nxstart)
{
    this->_nxstart = nxstart;
}

void ds::VolumeHeader2dx::set_nystart(int nystart)
{
    this->_nystart = nystart;
}

void ds::VolumeHeader2dx::set_nzstart(int nzstart)
{
    this->_nzstart = nzstart;
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
    _symmetry = volume::symmetrization::Symmetry2dx(symmetry);
}

void ds::VolumeHeader2dx::set_gamma(double gamma)
{
    _gamma = gamma;
}

void ds::VolumeHeader2dx::set_max_resolution(double resolution)
{
    _max_resolution = resolution;
}

int ds::VolumeHeader2dx::rows() const
{
    return _rows;
}

int ds::VolumeHeader2dx::columns() const
{
    return _columns;
}

int ds::VolumeHeader2dx::sections() const
{
    return _sections;
}

int ds::VolumeHeader2dx::mx() const
{
    return _mx;
}

int ds::VolumeHeader2dx::my() const
{
    return _my;
}

int ds::VolumeHeader2dx::mz() const
{
    return _mz;
}

int ds::VolumeHeader2dx::nxstart() const
{
    return _nxstart;
}

int ds::VolumeHeader2dx::nystart() const
{
    return _nystart;
}

int ds::VolumeHeader2dx::nzstart() const
{
    return _nzstart;
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
