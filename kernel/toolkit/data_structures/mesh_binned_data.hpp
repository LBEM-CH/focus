/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef MESH_BINNED_DATA_HPP
#define	MESH_BINNED_DATA_HPP

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

namespace tdx
{
    namespace data
    {   
        /**
         * A class used to store 2D binned data.
         * The data are provided at the x-points and y-points and the class 
         * will keep track of sums and counts in each bin
         * This class can be used to store the data and later plot the sums
         * or the simple averages of the stored data.
         */
        class MeshBinnedData
        {
        public:
            
            /**
             * Constructor with min, max range and number of bins assigned 
             * @param min_x
             * @param max_x
             * @param min_y
             * @param max_y
             * @param bins_x
             * @param bins_y
             */
            MeshBinnedData(double min_x, double max_x, double min_y, double max_y, int bins_x, int bins_y);
                       
            /**
             * Sets the value at the point provided
             * @param x
             * @param y
             * @param value
             */
            void add_data_at(double x, double y, double value);
            
           /**
            * Replaces the current sum in the bin with the one provided
            * @param bin_x
            * @param bin_y
            * @param sum
            */
            void set_bin_sum(int bin_x, int bin_y, double sum);
            
           /**
            * Replaces the current count in the bin with the one provided
            * @param bin_x
            * @param bin_y
            * @param count
            */
            void set_bin_count(int bin_x, int bin_y, int count);
            
            /**
             * Writes into a file the bins_x (1st column) bins_y (2nd column) 
             * and summed data (3rd column)
             * @param file - output file
             */
            void write_sum(std::string file) const;
            
            /**
             * Writes into a file the bins_x (1st column) bins_y (2nd column)
             * and averaged data (3rd column)
             * @param file - output file
             */
            void write_average(std::string file) const;
            
            /**
             * Writes into a file the bins_x (1st column) bins_y (2nd column)
             * and data (3rd column)
             * @param file - file name
             * @param average - should the data be averaged
             */
            void write(std::string file, bool average) const;
            
            /**
             * Fetches the summed value at the data point provided
             * @param x
             * @param y
             * @return double value at provided point
             *         Returns -1.0, if found inappropriate value
             */
            double sum_at(double x, double y) const;
            
            /**
             * Fetches the averaged value at the data point provided
             * @param x
             * @param y
             * @return double value at provided data point
             *         Returns -1.0, if found inappropriate value
             */
            double average_at(double x, double y) const;
            
            /**
             * Fetches the sum of the data in the provided bin
             * @param bin_x number starting from 0
             * @param bin_y number starting from 0
             * @return sum in bin
             *         for inappropriate bins returns -1
             */
            double sum_in(int bin_x, int bin_y) const;
            
            /**
             * Fetches the average of the data in the provided bin
             * @param bin_x number starting from 0
             * @param bin_y number starting from 0
             * @return average in bin
             *         for inappropriate bins returns -1
             */
            double average_in(int bin_x, int bin_y) const;
            
            /**
             * Returns the value of minimum range set in x.
             * @return min range set
             */
            double min_x() const;
            
             /**
             * Returns the value of maximum range set in x.
             * @return max range set
             */
            double max_x() const;
            
            /**
             * Returns the value of minimum range set in y.
             * @return min range set
             */
            double min_y() const;
            
             /**
             * Returns the value of maximum range set in y.
             * @return max range set
             */
            double max_y() const;
            
            /**
             * Returns the number of bins used in x
             * @return number of bins
             */
            int bins_x() const;
            
            /**
             * Returns the number of bins used in y
             * @return number of bins
             */
            int bins_y() const;
            
            /**
             * Fetches the bin spacing which was used 
             * to generate the data in x
             * @return spacing
             */
            double spacing_x() const;
            
             /**
             * Fetches the bin spacing which was used 
             * to generate the data in y
             * @return spacing
             */
            double spacing_y() const;
            
            /**
             * Fetches the correct bin number (starting 0) for the given
             * data point
             * @param x
             * @return bin number starting with 0,1,..,bins-1
             *         for inappropriate point values returns -1
             */
            int get_bin_x(double x) const;
            
             /**
             * Fetches the correct bin number (starting 0) for the given
             * data point
             * @param y
             * @return bin number starting with 0,1,..,bins-1
             *         for inappropriate point values returns -1
             */
            int get_bin_y(double y) const;
            
            
        private:
            
            /**
             * Fetches the 1D array id for the 2D bin id
             * @param bin_x
             * @param bin_y
             * @return bin number
             */
            int get_linear_bin(int bin_x, int bin_y) const;
            
               
            /**
             * Value of minimum range
             */
            double _min_x, _max_x;
            
            /**
             * Value of maximum range
             */
            double _min_y, _max_y;
            
            /**
             * Number of bins used to generate the data
             */
            int _bins_x, _bins_y;
            
            /**
             * Array of size <bins> used to store
             * data in each bin
             */
            double* _data;
            
            /**
             * Keeps a track of all the number of data points went to each bin
             */
            int* _counts;
            
        };
        
    }
    
}


#endif	/* BINNED_DATA_HPP */

