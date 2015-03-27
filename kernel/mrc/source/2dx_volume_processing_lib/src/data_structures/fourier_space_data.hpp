/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef FOURIERSPACEDATA_HPP
#define	FOURIERSPACEDATA_HPP

#include <map>
#include <list>

#include "miller_index.hpp"
#include "diffraction_spot.hpp"

namespace volume_processing_2dx
{
    namespace data_structures
    {
        /**
         * A class to store Fourier space data.
         * The data is stored as a map of miller indices and it's values.
         * Miller index is sorted with x changing slowest and z changing
         * fastest. For each miller index a complex value and it's weight
         * is also stored.
         */
        class FourierSpaceData
        {
            
        public:
            
            /**
             * Defines a const_iterator for the class
             */
            typedef std::map<MillerIndex, DiffractionSpot>::const_iterator const_iterator;
            
            /**
             * Default constructor initializing the data
             */
            FourierSpaceData();
            
            /**
             * Constructor using a multi-map of miller index and diffraction spot
             * Internally converts a multi-map of miller index to diffraction spots
             * to a map of miller index to diffraction spot. In the input
             * multi-map, there can exist multiple diffraction spots assigned to
             * one miller index, which will be converted to a map where only
             * one diffraction spot is assigned to a miller index.
             * @param spot_multimap: A multi-map with miller indices mapped to multiple complexes
             */
            FourierSpaceData(const std::multimap<MillerIndex, DiffractionSpot>& spot_multimap);
            
            /**
             * Returns the beginning of the data for iterating
             * @return const_iterator
             */
            const_iterator begin() const;
            
            /**
             * Returns the beginning of the data for iterating
             * @return const_iterator
             */
            const_iterator end() const;
            
            /**
             * Overwrites the value of a miller index.
             * Internally inserts the miller index and it's value to the map.
             * @param h
             * @param k
             * @param l
             * @param value - complex value
             * @param weight - fom weight in fraction 
             */
            void set_value_at(int h, int k, int l, Complex2dx value, double weight);
            
            /**
             * Fetches the complex number stored at miller index h, k, l.
             * If found returns the complex, otherwise returns 0 + 0i
             * @param h
             * @param k
             * @param l
             * @return complex value at h, k, l
             */
            Complex2dx complex_at(int h, int k, int l) const ;
            
            /**
             * Fetches the weight at the miller index h, k, l.
             * If found returns the value, other wise returns 0.
             * @param h
             * @param k
             * @param l
             * @return 
             */
            double weight_at(int h, int k, int l) const ;
            
            /**
             * Checks if a miller index exists in map
             * @param h
             * @param k
             * @param l
             * @return True/False
             */
            bool exists(int h, int k, int l) const ;
            
        private:      
            std::map<MillerIndex, DiffractionSpot>* _data;
            
            
        }; // class FourierSpace
        
    } // namespace data_structures
    
} // namespace volume_processing_2dx

#endif	/* FOURIERSPACEDATA_HPP */

