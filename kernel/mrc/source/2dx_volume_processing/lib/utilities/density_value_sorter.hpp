/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef DENSITY_VALUE_SORTER_HPP
#define	DENSITY_VALUE_SORTER_HPP

#include <iostream>

namespace tdx
{
    namespace utilities
    {
        /**
         * A class used to perform the sorting of the density values.
         * Can also be used to get the corresponding ids of the sorted values.
         */
        class DensityValueSorter
        {
        public:
            
            /**
             * Constructor with data and it's size
             * @param max_size
             * @param data
             */
            DensityValueSorter(int max_size, double* data);
            
            /**
             * Returns the sorted array id's
             * @return ids of the sorted array
             */
            int* get_sorted_ids();
            
            /**
             * Returns the sorted values
             * @return sorted values
             */
            double* get_sorted_values();
            
        private:
            /**
             * A class to store a pair of density and it's id
             */
            class DensityIdPair
            {
            public:
                DensityIdPair(double density, int id)
                {
                    this->density = density;
                    this->id = id;
                };
                
                /**
                 * For sorting purposes!
                 * @param rhs
                 * @return 
                 */
                bool operator<(const DensityIdPair& rhs) const
                {
                    return (this->density < rhs.density);
                };
                
                double density;
                int id;
            };
            
            /**
             * Sorts the data;
             */
            void sort_data_with_id();
            
            /**
             * Stores the raw data
             */
            double* data;
            
            /**
             * Stores the sorted data
             */
            double* sorted_data;
            
            /**
             * Stores the sorted ids
             */
            int* sorted_ids;
            
            /**
             * The size of the data
             */
            int max_size;
            
        };
        
    }
    
}


#endif	/* DENSITY_VALUE_SORTER_HPP */

