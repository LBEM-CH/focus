#include <stdexcept>
#include <math.h>

#include "../../include/SymmetryOperations.hpp"
#include "../../include/NumericalUtils.hpp"

SymmetryOperations::SymmetryOperations(int opIndex, int symCode) {
    if(opIndex > 29){
        std::out_of_range ( "Invalid value for symmetry operation: "+ std::to_string(opIndex));
    }
    this->index = opIndex;
    this->hChange = hChanges[index];
    this->kChange = kChanges[index];
    this->lChange = lChanges[index];

    this->phaseChange = phaseChanges[symCode][index];
}

MillerIndex SymmetryOperations::getNewMiller(MillerIndex& oldIndex) {
    
    int h_new = oldIndex.h();
    if(abs(hChange) == 1) h_new = oldIndex.h() * sign(hChange);
    else if (abs(hChange) == 2) h_new = oldIndex.k() * sign(hChange);
    else if (abs(hChange) == 3) h_new = (oldIndex.h() + oldIndex.k()) * sign(hChange);
    
    int k_new = oldIndex.k();
    if(abs(kChange) == 1) k_new = oldIndex.h() * sign(kChange);
    else if(abs(kChange) == 2) k_new = oldIndex.k() * sign(kChange);
    else if(abs(kChange) == 3) k_new = (oldIndex.h() + oldIndex.k()) * sign(kChange);
    
    int l_new = lChange*oldIndex.l();
    
    return MillerIndex(h_new, k_new, l_new);
    
}

bool SymmetryOperations::skipOperation() {
    bool result;
    if(phaseChange == 0) result = true;
    else result = false;
    return result;
}

double SymmetryOperations::getPhaseChange(double phase, const MillerIndex& index) {
    if(phaseChange == 1) return phase;
    else if(phaseChange == 2) return (phase + index.h()*M_PI);
    else if(phaseChange == 3) return (phase + index.k()*M_PI);
    else if(phaseChange == 4) return (phase + (index.h()+index.k())*M_PI);
    else if(phaseChange == 5) return (phase + index.l()*M_PI);
    else{
        throw std::out_of_range ( "Invalid value for phase change: "+ std::to_string(phaseChange));
    }
}


