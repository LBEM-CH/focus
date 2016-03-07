/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef DIFFRACTION_SPOT_HPP
#define	DIFFRACTION_SPOT_HPP

#include <iostream>
#include <stdexcept>
#include <list>
#include <string.h>

#include "../basics/complex.hpp"

namespace tdx
{
    namespace data
    {
        class PeakData;
        typedef std::list<PeakData> PeakList;
        
        /**
         * A class used to store the data of a Bragg Peak including it's value 
         * and weight. This would translate to amplitude, phase and a weight in
         * reflection file
         */
        class PeakData
        {
            
        public:
            
            /**
             * Default constructor with value assigned to (0 + 0i) 
             * and weight assigned to 0.0
             */
            PeakData();
            
            /**
             * Constructor with a value and weight
             * @param value
             * @param weight in fractions
             */
            PeakData(Complex value, double weight);
            
            /**
             * Resets to a averaged peak from a list of peaks.
             * @param peak list
             */
            void from_peak_list(const PeakList peaks);
            
            /**
             * Define = operator. Equates the weights and the values
             * @param rhs
             * @return this instance equated to rhs
             */
            PeakData& operator=(const PeakData& rhs);
            
            /**
             * Definition of + operator. 
             * Adds the complex values and does special averaging of the weights
             * using the xargs
             * @param rhs
             * @return added diffraction spot
             */
            PeakData operator+(const PeakData& rhs);
            
            /**
             * Declaration of multiplication of a double with Diffraction spot
             * @param factor
             * @return 
             */
            PeakData operator*( double factor);
            
            /**
             * Definition of == operator.
             * Checks for the equality of both value and weight
             * @param rhs
             * @return True/false
             */
            bool operator==(const PeakData& rhs) const;
            
            /**
             * Definition of < operator.
             * Checks if the amplitudes of the values are small, 
             * if equal goes for the higher weight.
             * @param rhs
             * @return 
             */
            bool operator<(const PeakData& rhs) const;
            
            /**
             * Gets the complex value of the spot
             * @return value
             */
            tdx::Complex value() const ;
            
            /**
             * Gets the weight of the spot
             * @return FOM weight in fractions
             */
            double weight() const ;
            
            /**
             * Gets the amplitude of the spot
             * @return amplitude
             */
            double amplitude() const;
            
            /**
             * Gets the intensity of the spot
             * @return intensity
             */
            double intensity() const;
            
            /**
             * Gets the phase of the spot
             * @return phase
             */
            double phase() const;
            
            /**
             * Assigner function of the value
             * @param value
             */
            void set_value(tdx::Complex value);
            
            /**
             * Assigner function of the weight
             * @param weight in fractions
             */
            void set_weight(double weight);
            
            
        private:
            
            /**
             * Initializer function of the members
             * @param value
             * @param weight
             */
            void initialize(tdx::Complex value, double weight);
            
            /**
             * Value of the peak
             */
            tdx::Complex _value;
            
            /**
             * weight of the peak
             */
            double _weight;
            
        };//class DiffractionSpots
        
    }//namespace data_structures
    
}//namespace volume_processing_2dx


#endif	/* DIFFRACTION_SPOT_HPP */

