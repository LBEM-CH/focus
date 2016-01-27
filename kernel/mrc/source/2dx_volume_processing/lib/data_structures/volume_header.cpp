
#include "volume_header.hpp"

#include "../utilities/angle_utilities.hpp"

namespace ds = tdx::data;

ds::VolumeHeader::VolumeHeader()
{
    initialize(0, 0, 0);
}

ds::VolumeHeader::VolumeHeader(int nx, int ny, int nz)
{
    initialize(nx, ny, nz);
}

void ds::VolumeHeader::initialize(int nx, int ny, int nz)
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
    set_gamma(tdx::utilities::angle_utilities::DegreeToRadian(90));
    set_symmetry("P1");

}

void ds::VolumeHeader::reset_size(int nx, int ny, int nz)
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

std::string ds::VolumeHeader::to_string() const
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
               std::to_string(tdx::utilities::angle_utilities::RadianToDegree(gamma())) + "\n";
    output += "\t|Symmetry: " + symmetry() + "\n";
    output += "\t|Start indices: " + std::to_string(nxstart()) + " , " + 
               std::to_string(nystart()) + " , " + std::to_string(nzstart()) + "\n";
    
    return output;
}

void ds::VolumeHeader::set_rows(int rows)
{
    this->_rows = rows;
}

void ds::VolumeHeader::set_columns(int columns)
{
    this->_columns = columns;
}

void ds::VolumeHeader::set_sections(int sections)
{
    this->_sections = sections;
}

void ds::VolumeHeader::set_mx(int mx)
{
    this->_mx = mx;
}

void ds::VolumeHeader::set_my(int my)
{
    this->_my = my;
}

void ds::VolumeHeader::set_mz(int mz)
{
    this->_mz = mz;
}

void ds::VolumeHeader::set_nxstart(int nxstart)
{
    this->_nxstart = nxstart;
}

void ds::VolumeHeader::set_nystart(int nystart)
{
    this->_nystart = nystart;
}

void ds::VolumeHeader::set_nzstart(int nzstart)
{
    this->_nzstart = nzstart;
}

void ds::VolumeHeader::set_xlen(double xlen)
{
    this->_xlen = xlen;
}

void ds::VolumeHeader::set_ylen(double ylen)
{
    this->_ylen = ylen;
}

void ds::VolumeHeader::set_zlen(double zlen)
{
    this->_zlen = zlen;
}

void ds::VolumeHeader::set_symmetry(std::string symmetry)
{
    _symmetry = tdx::symmetrization::Symmetry2dx(symmetry);
}

void ds::VolumeHeader::set_gamma(double gamma)
{
    _gamma = gamma;
}

int ds::VolumeHeader::rows() const
{
    return _rows;
}

int ds::VolumeHeader::columns() const
{
    return _columns;
}

int ds::VolumeHeader::sections() const
{
    return _sections;
}

int ds::VolumeHeader::mx() const
{
    return _mx;
}

int ds::VolumeHeader::my() const
{
    return _my;
}

int ds::VolumeHeader::mz() const
{
    return _mz;
}

int ds::VolumeHeader::nxstart() const
{
    return _nxstart;
}

int ds::VolumeHeader::nystart() const
{
    return _nystart;
}

int ds::VolumeHeader::nzstart() const
{
    return _nzstart;
}

double ds::VolumeHeader::xlen() const
{
    return _xlen;
}

double ds::VolumeHeader::ylen() const
{
    return _ylen;
}

double ds::VolumeHeader::zlen() const
{
    return _zlen;
}

double ds::VolumeHeader::gamma() const
{
    return _gamma;
}

std::string ds::VolumeHeader::symmetry() const
{
    return _symmetry.symmetry_string();
}

int ds::VolumeHeader::symmetry_2dx_code() const
{
    return _symmetry.symmetry_code();
}

int ds::VolumeHeader::symmetry_ccp4_code() const
{
    return _symmetry.ccp4_index();
}
