#!/bin/bash

rm -f *.o
rm -f Test/Test.out
rm -f libSP.a

echo ' '
echo '=========================================='
echo ' '
echo '   doxygen'
echo '=========================================='
echo ' '

doxygen SingleParticle_doxygen

INC='-I/opt/local/include'
LIB='-L.. -L/opt/local/lib'
LIBS='-lSP -lcppunit -lfftw3f'

#FLAG='-fopenmp -pedantic -W -Wall -g3 -fno-inline' #-Werror
FLAG='-fopenmp -O3 -funroll-loops -pedantic -W -Wall' #-Werror


echo ' '
echo '=========================================='
echo ' '
echo '   building'
echo '=========================================='
echo ' '

TARGET=Config/ConfigContainer.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Datastructures/Orientation.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Datastructures/GlobalParticleInformation.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Datastructures/ParticleShift.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Datastructures/Particle.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Datastructures/ParticleContainer.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Datastructures/Projection2d.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Datastructures/Reconstruction3d.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Utilities/FFTCalculator.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Utilities/FFTPlanContainer.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Methods/PeakSearchMethods/MaxSearchMethod.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Methods/RefinementMethods/CCRefinementMethod.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Methods/FindBestProjectionMethods/CCFindBestProjectionMethod.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Methods/ProjectionMethods/RealSpaceProjectionMethod.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Methods/RestoreMissingConeMethods/HIORestoreMissingConeMethod.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Methods/Interpolate2dMethods/LinearInterpolate2dMethod.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Methods/Interpolate2dMethods/QuadraticInterpolate2dMethod.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

TARGET=Methods/CalcCCMethods/BasicCalcCCMethod.cpp
echo $TARGET
g++ $FLAG -c $INC $TARGET

echo ' '
echo '=========================================='
echo ' '
echo '   making library'
echo '=========================================='
echo ' ' 

ar rcs libSP.a ConfigContainer.o Orientation.o GlobalParticleInformation.o ParticleShift.o Particle.o ParticleContainer.o Projection2d.o Reconstruction3d.o RealSpaceProjectionMethod.o FFTCalculator.o FFTPlanContainer.o MaxSearchMethod.o CCFindBestProjectionMethod.o CCRefinementMethod.o HIORestoreMissingConeMethod.o LinearInterpolate2dMethod.o QuadraticInterpolate2dMethod.o BasicCalcCCMethod.o

echo ' '
echo '=========================================='
echo ' '
echo '   building test'
echo '=========================================='
echo ' '

cd Test
g++ $INC $FLAG TestRunner2.cpp -o Test.out $LIB $LIBS

echo ' '
echo '=========================================='
echo ' '
echo '   start testing'
echo '=========================================='
echo ' '

time ./Test.out
