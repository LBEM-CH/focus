/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef MILLER_INDEX_HPP
#define	MILLER_INDEX_HPP

#include <iostream>
#include <string.h>

namespace tdx
{
    namespace data
    {
        /**
         * A class to store and use Miller Index operations.
         * A miller index is a set of three integers which provides
         * in reciprocal space the identification of a reflection.
         */
        class MillerIndex
        {
            
        public:
            /**
             * Default constructor.
             * Initializes the indices to 0
             */
            MillerIndex();
            
            /**
             * Constructor with miller indices h, k, l
             * @param h
             * @param k
             * @param l
             */
            MillerIndex(int h, int k, int l);
            
            /**
             * Copy constructor
             * @param copy
             */
            MillerIndex(const MillerIndex& copy);
            
            /**
             * Equates this with the rhs
             * @param rhs
             * @return this equated with rhs
             */
            MillerIndex& operator=(const MillerIndex& rhs);
            
            /**
             * Checks the equality of all indices with that of rhs
             * @param rhs
             * @return True/False
             */
            bool operator==(const MillerIndex& rhs) const;
            
            /**
             * Defines the < operator.
             * Sorts in the following order: 
             * First h followed by k and followed by l
             * @return 
             */
            bool operator<(const MillerIndex& rhs) const;
            
            /**
             * Definition of <<
             * Modifies the output stream with "h k l"
             * @param os
             * @return 
             */
            std::ostream& operator<<(std::ostream& os) const;
            
            /**
             * Getter method for index h (x-dimension).
             * @return h of Miller Index 
             */
            int h() const;
            
            /**
             * Getter method for index k (y-dimension).
             * @return k of Miller Index 
             */
            int k() const;
            
            /**
             * Getter method for index l (z-dimension).
             * @return l of Miller Index 
             */
            int l() const;
            
            /**
             * Sets the indices.
             * @param h
             * @param k
             * @param l
             */
            void set_values(int h, int k, int l);
            
            /**
             * Returns a string demonstrating current miller index
             * which can be used for output purpose
             * @return 
             */
            std::string to_string() const;
            
            /**
             * Fetches the miller index of Friedel spot.
             * A Friedel spot is the indices multiplied by -1.
             * @return 
             */
            MillerIndex FriedelSpot() const;
            
            
        private:
            /**
             * Member initializer function with int h, k, l
             * @param h
             * @param k
             * @param l
             */
            void initialize(int h, int k, int l);
            
            /**
             * Members
             */
            int _h, _k, _l;
            
            
            
            
        }; // class MillerIndex
        
    } // namespace data_structures
    
} // namespace volume_processing_2dx

#endif	/* MILLER_INDEX_HPP */

