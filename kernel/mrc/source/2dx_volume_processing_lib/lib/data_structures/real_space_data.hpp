/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#ifndef REAL_SPACE_DATA_HPP
#define	REAL_SPACE_DATA_HPP

namespace volume
{
    namespace data
    {
        /**
         * A class to sore real space data in a double array format.
         */
        class RealSpaceData
        {
        public:
            
            /**
             * Default constructor initializing a 0 size data
             */
            RealSpaceData();
            
            /**
             * Constructor initializing the data with given size
             * @param nx
             * @param ny
             * @param nz
             */
            RealSpaceData(int nx, int ny, int nz);
            
            /**
             * Resets the data with the given size
             * @param nx
             * @param nx
             * @param nz
             */
            void reset(int nx, int ny, int nz);
            
            /**
             * Resets the data. make sure that the size of the data is equals
             * the size of the class
             * @param data
             */
            void reset_data(double* data);
            
            /**
             * Returns the pointer to the data
             * @return pointer to the data
             */
            double* get_data() const;
            
            /**
             * Fetches the value at the given index
             * @param x
             * @param y
             * @param z
             * @return value
             */
            double get_value_at(int x, int y, int z) const ;
            
             /**
             * Fetches the value at the given index
             * @param id
             * @return value
             */
            double get_value_at(int id) const ;
            
            /**
             * Assigns the value to the given index
             * @param x
             * @param y
             * @param z
             * @param value
             */
            void set_value_at(int x, int y, int z, double value);
            
            /**
             * Assigns the value to the given index
             * @param id
             * @param value
             */
            void set_value_at(int id, double value);
            
            /**
             * x-size of the data
             * @return x-size of the data
             */
            int nx() const;
            
            /**
             * y-size of the data
             * @return y-size of the data
             */
            int ny() const;
            
            /**
             * z-size of the data
             * @return z-size of the data
             */
            int nz() const;
            
            /**
             * Evaluates minimum density value
             * @return minimum density value
             */
            double min() const;
            
            /**
             * Evaluates maximum density value
             * @return maximum density value
             */
            double max() const;
            
            /**
             * Evaluates mean density value
             * @return mean density value
             */
            double mean() const;
            
            /**
             * The the current size of the array
             * @return size
             */
            long size() const ;
            
             /**
             * Fetches the memory location of the x, y, z
             * @param x
             * @param y
             * @param z
             * @return 
             */
            int memory_id(int x, int y, int z) const ;
            
            /**
             * Merges the data from other real spaced data. The input data's 
             * center will be placed at the location x, y, z in the current data
             * @param to_be_merged : Real space data to be merged
             * @param x - Location x where the input map's x-center will be merged
             * @param y - Location y where the input map's y-center will be merged
             * @param z - Location z where the input map's z-center will be merged
             */
            void merge_data(const RealSpaceData& to_be_merged, int x, int y, int z);
            
            /**
             * Sorts the real space data and returns the sorted id's
             * @return the sorted id's
             */
            int* density_sorted_ids();
            
            /**
             * Sorts the real space data according to density values
             * and returns the values
             * @return 
             */
            double* density_sorted_values();
            
            /**
             * Applies a density slab in the vertical direction (z-axis) with the
             * fractional height. The new densities are changed only with the 
             * fraction provided. fraction = 1.0 will completely change the map
             * @param height: height in fraction of z height
             * @param fraction: fraction, by which the densities are changed
             * @param centered: Is the density centered along z-axis?
             */
            void vertical_slab(double height, double fraction, bool centered);
            
            /**
             * Apply a density threshold. fraction = 1.0 will completely remove
             * densities below the limit
             * @param limit
             * @param fraction
             */
            void threshold(double limit = 0.0, double fraction=1.0);
            
        private:
            /**
             * A member initializer function with the size integers
             * @param nx
             * @param ny
             * @param nz
             */
            void initialize(int nx, int ny, int nz);
            
            
            /**
             * Checks if the indices are correct with the given data!
             * Should bypass the seg fault 11!
             * @param x
             * @param y
             * @param z
             * @return True for correct, false otherwise
             */
            bool indices_in_limit(int x, int y, int z) const ;
            
            /**
             * Data variable as a pointer to the double array
             */
            double* _data;
            
            /**
             * The size of the current data variable
             */
            int _nx, _ny, _nz;
            
            
        }; //class RealSpaceData
        
    } // namespace data_structures
    
} // namespace volume_processing_2dx

#endif	/* REAL_SPACE_DATA_HPP */

