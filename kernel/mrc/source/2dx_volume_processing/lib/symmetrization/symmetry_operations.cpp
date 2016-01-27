/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "symmetry_operations.hpp"
#include "../utilities/number_utilities.hpp"


namespace sym = tdx::symmetrization;

sym::SymmetryOperations::SymmetryOperations(int operator_index, int symmetry_code) 
{
    this->initialize(operator_index, symmetry_code);
}

void sym::SymmetryOperations::initialize(int operator_index, int symmetry_code)
{
    if (operator_index > 29) 
    {
        throw std::out_of_range ( "Invalid value for symmetry operation: "+ std::to_string(operator_index));
    }
    else if (symmetry_code > 16) 
    {
        throw std::out_of_range ( "Invalid code for symmetry: "+ std::to_string(symmetry_code));
    }
    else{
        this->index = operator_index;
        this->hChange = hChanges[index];
        this->kChange = kChanges[index];
        this->lChange = lChanges[index];
        this->phaseChange = phaseChanges[symmetry_code][index];
    }
}

void sym::SymmetryOperations::SymmetricMillerIndex(int* h, int* k, int* l) const
{
    //for sign
    namespace nu = tdx::utilities::number_utilities;
    
    int h_old = *h;
    int k_old = *k;
    int l_old = *l;
    
    if(abs(hChange) == 1)
    {
        *h = h_old * nu::Sign(hChange);
    }
    else if (abs(hChange) == 2) 
    {
        *h = k_old * nu::Sign(hChange);
    }
    else if (abs(hChange) == 3) 
    {
        *h = (h_old + k_old) * nu::Sign(hChange);
    }
    
    
    if(abs(kChange) == 1)
    { 
        *k = h_old * nu::Sign(kChange);
    }
    else if(abs(kChange) == 2) 
    {
        *k = k_old * nu::Sign(kChange);
    }
    else if(abs(kChange) == 3) 
    {
        *k = (h_old + k_old) * nu::Sign(kChange);
    }
    
    *l = lChange * l_old;
    
}

bool sym::SymmetryOperations::SkipOperation() const
{
    bool result;
    if(phaseChange == 0) result = true;
    else result = false;
    return result;
}

double sym::SymmetryOperations::PhaseChange(const double phase, const int h, const int k, const int l) const
{
    if(phaseChange == 1)      return phase;
    else if(phaseChange == 2) return (phase + h * M_PI);
    else if(phaseChange == 3) return (phase + k * M_PI);
    else if(phaseChange == 4) return (phase + (h + k) * M_PI);
    else if(phaseChange == 5) return (phase + l * M_PI);
    else
    {
        throw std::out_of_range ( "Invalid value for phase change: "+ std::to_string(phaseChange));
    }

}


