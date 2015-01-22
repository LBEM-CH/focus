#include <iostream>
#include <list>
#include <complex>
#include <fstream>
#include <math.h>

#include "VolumeHKL.hpp"
#include "NumericalUtils.hpp"

VolumeHKL::VolumeHKL(int nx, int ny, int nz, std::string symmetry="P1", double apix=1.0, double max_resolutuion=2.0, double amp_epsilon=0.001) {
    this->nx = nx;
    this->ny = ny;
    this->nz = nz;
    this->symmetry = new Symmetry2dx(symmetry);
    this->apix = apix;
    this->max_resolution = max_resolutuion;
    this->amp_epsilon = amp_epsilon;
}

VolumeHKL::VolumeHKL(const VolumeHKL& other) {
    this->reflections = other.reflections;
    this->nx = other.nx;
    this->ny = other.ny;
    this->nz = other.nz;
    this->symmetry = other.symmetry;
    this->apix = other.apix;
    this->max_resolution = other.max_resolution;
    this->amp_epsilon = other.amp_epsilon;
    this->symmetrized = other.symmetrized;
}

double VolumeHKL::getResolution(const MillerIndex& index) {
    double res[3];
    
    res[0] = res[1] = res[2] = 1000;
    
    double nyquist = 1/(2*apix);
    
    if(index.h!=0) res[0] = nx/(2*nyquist*index.h);
    if(index.k!=0) res[1] = ny/(2*nyquist*index.k);
    if(index.l!=0) res[2] = nz/(2*nyquist*index.l);
    
    return sqrt(res[0]*res[0] + res[1]*res[1] + res[2]*res[2]);
}


void VolumeHKL::addHKZData(std::ifstream& hkzFile){
    
    ReflectionMultiMap refMultiMap;
    int hIn, kIn, lIn, iqIn;
    double zIn, ampIn, phaseIn, sampIn, sphaseIn;
    double scale = 10000.0;
    while (hkzFile >> hIn >> kIn >> zIn >> ampIn >> phaseIn >> sampIn >> sphaseIn >> iqIn)
    {
        // double dz = apix/nz;
        // double dz = 1.0/(nz*apix);
        // double dz = 1.0/nz;
        // lIn = round( (zIn/dz) );
        lIn = round( zIn * nz );
        
        MillerIndex *indexIn = new MillerIndex(hIn, kIn, lIn);
        double resolution =  getResolution(*indexIn);
        
        //Convert phase to radians and Shift the phase so that protein is in center of z
        phaseIn += lIn*180;
        phaseIn *= (M_PI/180);
        
        //Covert also the sphaseIn to radians
        if(sphaseIn>90) sphaseIn = 0;
        sphaseIn *= (M_PI/180);
        
        if(resolution > max_resolution){
            double weightIn = cos(sphaseIn);
            double realIn = scale*weightIn*ampIn*cos(phaseIn);
            double imagIn = scale*weightIn*ampIn*sin(phaseIn);
            refMultiMap.insert(MI_Reflection_Pair(*indexIn, Reflection(Complex2dx(realIn, imagIn), weightIn)));
        }
            
    }
    
    this->reflections = averageOutReflections(refMultiMap);
}

ReflectionMap VolumeHKL::averageOutReflections(ReflectionMultiMap refMultiMap) {
    ReflectionMap averagedMap;
    bool initialized = false;
    
    MillerIndex currentIndex;
    ReflectionList reflectionsCurrent;
    for(ReflectionMultiMap::const_iterator ii=refMultiMap.begin(); ii!=refMultiMap.end(); ++ii){
        if(!(initialized)){
            currentIndex = (*ii).first;
            initialized = true;
        }
        
        if(!(currentIndex == (*ii).first)){
            //std::cout << "Averaging: " << currentIndex <<" ";
            averagedMap[currentIndex] = averageReflectionsFromList(reflectionsCurrent);   
            reflectionsCurrent.clear();
        }
        reflectionsCurrent.push_back((*ii).second);
        currentIndex = (*ii).first;
        
    }
    
    averagedMap[currentIndex] = averageReflectionsFromList(reflectionsCurrent); 
    
    return averagedMap;
}


void VolumeHKL::setMillerIndex(MillerIndex index, std::complex<double> value, double weight) {
    reflections.insert(MI_Reflection_Pair(index, Reflection(value, weight)));
}

std::ostream& operator<<(std::ostream& os, const VolumeHKL& volume){
    
    os << "2dx HKL VOLUME:\n";
    os << "Size: " << volume.nx << " X " << volume.ny << " X " << volume.nz << "\n";
    os << "Number of non unique reflections: " << volume.reflections.size() << "\n";
    
    return os;

}

void VolumeHKL::writeHKL(const char* fileName) {
    
    std::FILE* hklFile;
    hklFile = fopen(fileName, "w");
    for(ReflectionMap::const_iterator ii=reflections.begin(); ii!=reflections.end(); ++ii){
        fprintf(hklFile, "%5d %5d %5d %11.5f %12.7f %12.7f\n", 
                (*ii).first.h, (*ii).first.k, (*ii).first.l, (*ii).second.getAmplitude(), ((*ii).second.getPhase())*180/M_PI, (*ii).second.weight*100);
    }
    fclose(hklFile);
    
}

void VolumeHKL::symmetrize() {
    ReflectionMultiMap refMultiMap;

    for(ReflectionMap::iterator ref=reflections.begin(); ref!=reflections.end(); ++ref){
        if((*ref).second.getAmplitude() > amp_epsilon){
            refMultiMap.insert(MI_Reflection_Pair((*ref).first, (*ref).second));
            for(int opIndex=0; opIndex<30; opIndex++){
                SymmetryOperations operation = SymmetryOperations(opIndex, *symmetry);
                if(!(operation.skipOperation())){
                   MillerIndex* newIndex = operation.getNewMiller((*ref).first);
                   double newPhase = operation.getPhaseChange((*ref).second.getPhase(), (*ref).first);
                   //For negative h get Friedel spot, change phase
                   if(newIndex->h<0){
                       newIndex = newIndex->getFriedelSpot();
                       newPhase = -1*newPhase;
                   }
                   
                   Reflection newReflection = Reflection((*ref).second);
                   newReflection.setPhase(newPhase);
                   refMultiMap.insert(MI_Reflection_Pair(*newIndex, newReflection));
                }
            }
        }
    }

    reflections.clear();
    reflections = averageOutReflections(refMultiMap);

    symmetrized = true;
}
