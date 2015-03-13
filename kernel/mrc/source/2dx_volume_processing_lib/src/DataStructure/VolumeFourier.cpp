/*
 * Routines implementing the Fourier volume
 * 
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 */

#include "../../include/VolumeFourier.hpp"

void VolumeFourier::read_volume(std::string hkzFileName) {
    std::multimap<Triplet2dx, WeightedValue<Complex2dx> > refMultiMap;
    
    int hIn, kIn, lIn, iqIn;
    double zIn, ampIn, phaseIn, sampIn, sphaseIn;
    
    std::ifstream hkzFile(hkzFileName);
    
    //Start reading in the file
    while (hkzFile >> hIn >> kIn >> zIn >> ampIn >> phaseIn >> sampIn >> sphaseIn >> iqIn)
    {
        lIn = round(zIn * get_header().nz);
        double resolution =  get_resolution(hIn, kIn, lIn);
        
        //Convert phase to radians
        phaseIn *= (M_PI/180);
        
        //Covert also the sphaseIn to radians
        if(sphaseIn>90) sphaseIn = 0;
        sphaseIn *= (M_PI/180);
        
        if(resolution > get_header().max_resolution){
            double weightIn = cos(sphaseIn);
            double realIn = weightIn*ampIn*cos(phaseIn);
            double imagIn = weightIn*ampIn*sin(phaseIn);
            Triplet2dx indexIn = Triplet2dx(hIn, kIn, lIn);
            Complex2dx complexIn = Complex2dx(realIn, imagIn);
            WeightedValue<Complex2dx> valueIn = WeightedValue<Complex2dx>(complexIn, weightIn);
            //std::cout << indexIn.x() << " " <<indexIn.y() << " " << indexIn.z() << " : " << valueIn.getValue().getAmplitude() << valueIn.getValue().getAmplitude() << "\n";
            refMultiMap.insert(std::pair<Triplet2dx, WeightedValue<Complex2dx> > (indexIn, valueIn));
        }
            
    }
    
    hkzFile.close();
    
    _data = averageOutReflections(refMultiMap);
    
    //Scale the amplitudes, so that maximum is 20,000
    double currentMax = get_maximum().getValue().getAmplitude();
    this->scale(20000/currentMax);
    
    /*
    for(std::map<Triplet2dx, WeightedComplex >::const_iterator ii=data.begin(); ii!=data.end(); ++ii){
        int h = (*ii).first.x();
        int k = (*ii).first.y();
        int l = (*ii).first.z();
        double amp = (*ii).second.getValue().getAmplitude();
        double phase = ((*ii).second.getValue().getPhase())*180/M_PI;
        double fom = (*ii).second.getWeight()*100;
        
        std::cout << h << " " << k << " " << l << " " << amp << " " << " " << phase << " " << fom << "\n";
    }
    */
}

void VolumeFourier::write_volume(const char* hklFileName) {
    std::FILE* hklFile;
    
    hklFile = fopen(hklFileName, "w");
    for(std::map<Triplet2dx, WeightedComplex >::const_iterator ii=_data.begin(); ii!=_data.end(); ++ii){
        int h = (*ii).first.x();
        int k = (*ii).first.y();
        int l = (*ii).first.z();
        double amp = (*ii).second.getValue().getAmplitude();
        double phase = ((*ii).second.getValue().getPhase())*180/M_PI;
        double fom = (*ii).second.getWeight()*100;
        
        fprintf(hklFile, "%5d %5d %5d %11.5f %12.7f %12.7f\n", h, k, l, amp, phase, fom);
    }
    fclose(hklFile);
}


void VolumeFourier::symmetrize() {
    std::multimap<Triplet2dx, WeightedComplex > refMultiMap;

    for(std::map<Triplet2dx, WeightedComplex >::iterator ref=_data.begin(); ref!=_data.end(); ++ref){
        if((*ref).second.getValue().getAmplitude() > 0.0001){
            
            //Put in the original reflection
            refMultiMap.insert(std::pair<Triplet2dx, WeightedValue<Complex2dx> >((*ref).first, (*ref).second));
            
            //Loop over all possible operations
            Symmetry2dx sym2dx = Symmetry2dx(get_header().symmetry);
            for(int opIndex=0; opIndex<30; opIndex++){
                SymmetryOperations operation = SymmetryOperations(opIndex, sym2dx);
                if(!(operation.skipOperation())){
                    MillerIndex oldIndex = MillerIndex((*ref).first);
                    WeightedComplex oldReflection = WeightedComplex((*ref).second);
                    double oldAmplitude = oldReflection.getValue().getAmplitude();
                    double oldPhase = oldReflection.getValue().getPhase();
                    
                    //Get the symmetric miller index for this operator
                    MillerIndex newIndex = operation.getNewMiller(oldIndex);
                    
                    //Get the corresponding new phase
                    double newPhase = operation.getPhaseChange(oldPhase, oldIndex);
                    
                    //For negative h get Friedel spot, change phase
                    if(newIndex.h()<0){
                       newIndex = newIndex.getFriedelSpot();
                       newPhase = -1*newPhase;
                    }
                    
                    double newReal = oldAmplitude*cos(newPhase);
                    double newImag = oldAmplitude*sin(newPhase);
                   
                    WeightedComplex newReflection = WeightedComplex(Complex2dx(newReal, newImag), oldReflection.getWeight());
                    
                    //std::cout << opIndex << "(" << oldIndex.h() << "," << oldIndex.k() << "," << oldIndex.l() <<")";
                    //std::cout << " -> (" << newIndex->h() << "," << newIndex->k() << "," << newIndex->l() <<")";
                    //std::cout << " phase " << (*ref).second.getValue().getPhase() << "->" << newReflection.getValue().getPhase() <<"\n";
                    
                    Triplet2dx new_triplet = Triplet2dx(newIndex.h(), newIndex.k(), newIndex.l());
                    refMultiMap.insert(std::pair<Triplet2dx, WeightedValue<Complex2dx> >(new_triplet, newReflection));
                }
            }
        }
    }

    _data.clear();
    _data = averageOutReflections(refMultiMap);
}

double VolumeFourier::get_resolution(int x, int y, int z) const {
    double res[3];
    
    res[0] = res[1] = res[2] = 1000;
    
    double nyquist = 1/(2*get_header().apix);
    
    if(x!=0) res[0] = get_header().nx/(2*nyquist*x);
    if(y!=0) res[1] = get_header().ny/(2*nyquist*y);
    if(z!=0) res[2] = get_header().nz/(2*nyquist*z);
    
    return sqrt(res[0]*res[0] + res[1]*res[1] + res[2]*res[2]);
}



std::map<Triplet2dx, WeightedComplex> VolumeFourier::averageOutReflections(std::multimap<Triplet2dx, WeightedComplex> refMultiMap) {
    std::map<Triplet2dx, WeightedComplex> averagedMap;
    bool initialized = false;
    
    Triplet2dx currentIndex;
    std::list<WeightedValue<Complex2dx> > reflectionsCurrent;
    
    for(std::multimap<Triplet2dx, WeightedComplex>::const_iterator ii=refMultiMap.begin(); ii!=refMultiMap.end(); ++ii){
        if(!(initialized)){
            currentIndex = (*ii).first;
            initialized = true;
        }
        
        if(!(currentIndex == (*ii).first)){
            WeightedComplex avgedReflection = averageFromList(reflectionsCurrent);
            averagedMap.insert(std::pair<Triplet2dx, WeightedComplex> (currentIndex, avgedReflection));
            //std::cout << currentIndex.x() << " " << currentIndex.y() << " " << currentIndex.z() << "\n";
            //std::cout << " : " << avgedReflection.getValue().real() << "," << avgedReflection.getValue().imag();
            //WeightedComplex getback = averagedMap[currentIndex];
            //std::cout << " : " << getback.getValue().real() << "," << getback.getValue().imag() <<"\n";
            reflectionsCurrent.clear();
        }
        reflectionsCurrent.push_back((*ii).second);
        currentIndex = (*ii).first;
        
    }
    
    averagedMap.insert(std::pair<Triplet2dx, WeightedComplex> (currentIndex, averageFromList(reflectionsCurrent))); 
    
    return averagedMap;
}

WeightedValue<Complex2dx> VolumeFourier::averageFromList(std::list<WeightedValue<Complex2dx> > values) {
    WeightedValue<Complex2dx> sumOverall;
    Complex2dx sumValues(0,0);
    double sumWeight = 0.0;
    double sumXarg = 0.0;
    for(std::list<WeightedValue<Complex2dx> >::const_iterator i=values.begin(); i != values.end(); ++i){
        //std::cout << (*i).getValue().real() << " " << (*i).getValue().imag() <<"\n" ;
        sumWeight += (*i).getWeight();
        sumXarg += fomToXarg((*i).getWeight());
        sumValues = sumValues + (*i).getValue();
    }
    
    if(sumXarg > 54) sumXarg = 54;
    double xargAvg = i1(sumXarg)/i0(sumXarg);
    
    //std::cout << "sums: " << sumWeight << " " << xargAvg << " (" << sumValues.real() << "," << sumValues.imag() << ")\n";
    
    if(sumWeight!=0 && values.size()!=0){
        //std::cout << "In loop\n";
        sumValues = sumValues*(xargAvg/sumWeight);
        //std::cout << " (" << sumValues.real() << "," << sumValues.imag() << ")\n";
        sumOverall.setValue(sumValues);
        sumOverall.setWeight(xargAvg);
    }
    
    
    //std::cout << "-------------\n" << sumOverall.getValue().real() << " " << sumOverall.getValue().imag() << "\n--------------\n";
    return sumOverall;  
}

fftw_complex* VolumeFourier::to_fftw_complex() const {
    
    //Calculate the size of the array
    Triplet2dx array_dim(_header.nx/2+1, _header.ny, _header.nz);
    int vol_size = array_dim.get_self_muliplication();
    
    //Assign memory to output data
    // NOTE: Assumption, z dimension is always even
    fftw_complex* fftw_data;
    fftw_data = (fftw_complex*) malloc(vol_size*sizeof(fftw_complex));
    
    //Initialize the fftw_data with 0
    for(int i=0; i<vol_size; ++i){
        ((fftw_complex*)fftw_data)[i][0] = 0.0;
        ((fftw_complex*)fftw_data)[i][1] = 0.0;
    }
    
    //std::cout << "Size " << array_dim.x() << " " << array_dim.y() << " " << array_dim.z() << "\n";
    //Iterate over all possible miller indices h, k, l
    for(std::map<Triplet2dx, WeightedComplex >::const_iterator ref=_data.begin(); ref!=_data.end(); ++ref){
        
        //Assign the current Miller Index to the array
        MillerIndex currentHKL = MillerIndex((*ref).first.x(), (*ref).first.y(), (*ref).first.z());
        Complex2dx currentComplex = (*ref).second.getValue();
        // Fill in this spot
        if(currentHKL.h() >= 0){
            Triplet2dx indexTriplet = currentHKL.to_array_index(array_dim);
            int arrayId = indexTriplet.get_array_id(array_dim);
            ((fftw_complex*)fftw_data)[arrayId][0] = currentComplex.real();
            ((fftw_complex*)fftw_data)[arrayId][1] = currentComplex.imag();
        }
        
        // Fill in the Friedel spot
        //MillerIndex friedelHKL = currentHKL.getFriedelSpot();
        //Complex2dx friedelComplex(currentComplex.real(), -1*currentComplex.imag());
        //if(friedelHKL.l() >= 0){
        //    Triplet2dx indexTriplet = friedelHKL.to_array_index(array_dim);
        //    int arrayId = indexTriplet.get_array_id(array_dim);
        //    ((fftw_complex*)fftw_data)[arrayId][0] = friedelComplex.real();
        //    ((fftw_complex*)fftw_data)[arrayId][1] = friedelComplex.imag();
        //}
        
        //std::cout << "Setting " << indexTriplet.x() << ", " << indexTriplet.y() << ", " << indexTriplet.z() << " for "
        //                      << currentHKL.h() << ", " << currentHKL.k() << ", " << currentHKL.l() <<" as "
        //                      << currentComplex.real() << " " << currentComplex.imag() <<"\n";
    }
    
    return fftw_data;
    
}

void VolumeFourier::from_fftw_complex(const fftw_complex* complex_data) {
    _data.clear();
    
    //Calculate the size of the array
    Triplet2dx array_dim(_header.nx/2+1, _header.ny, _header.nz);
    
    //Assuming nx, ny to be odd and nz to be even
    MillerIndex index_max(array_dim.x()-1, array_dim.y()/2, array_dim.z()/2);
    //std::cout << "Max Miller: " << index_max.h() << " " << index_max.k() << " " << index_max.l() << "\n";
    
    //Loop over all possible indices and set their values
    for(int ix=0; ix<array_dim.x(); ix++){
        for(int iy=0; iy<array_dim.y(); iy++){
            for(int iz=0; iz<array_dim.z(); iz++){
                Triplet2dx currArray(ix, iy, iz);
                int currId = currArray.get_array_id(array_dim);
                double real = ((fftw_complex*)complex_data)[currId][0];
                double imag = ((fftw_complex*)complex_data)[currId][1];
                
                //Assign the current miller index
                Complex2dx currentComplex(real, imag);
                MillerIndex currentHKL(currArray, array_dim, index_max);
                if(currentHKL.h() >= 0 && currentComplex.getAmplitude() > 0.0001){
                    WeightedComplex wtdValue(currentComplex, 1.0);
                    this->set_value_at(currentHKL.h(), currentHKL.k(), currentHKL.l(), wtdValue);
                }
                
                //Assign the friedel miller index
                //MillerIndex friedelHKL = currentHKL.getFriedelSpot();
                //Complex2dx friedelComplex(real, -1*imag);
                //if(friedelHKL.h() >= 0 && friedelComplex.getAmplitude() > 0.0001){
                //    WeightedComplex wtdValue(friedelComplex, 1.0);
                //    this->set_value_at(friedelHKL.h(), friedelHKL.k(), friedelHKL.l(), wtdValue);
                //}
                //if(currComplex.getAmplitude() > 0.0001){
                //    WeightedComplex wtdValue(currComplex, 1.0);
                    //std::cout << "For " << ix << ", " << iy << ", " << iz << " setting "
                    //          << currMiller.h() << ", " << currMiller.k() << ", " << currMiller.l() <<" as "
                    //          << currComplex.real() << " " << currComplex.imag() <<"\n";
                //  set_value_at(currMiller.h(), currMiller.k(), currMiller.l(), wtdValue);
                //}
            }
        }
    }
}



