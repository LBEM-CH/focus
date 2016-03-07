/* 
 * File:   volume_list.hpp
 * Author: biyanin
 *
 * Created on February 25, 2016, 12:16 PM
 */

#ifndef VOLUME_STACK_HPP
#define	VOLUME_STACK_HPP

#include <iostream>
#include <vector>
#include <string>

#include "volume2dx.hpp"

namespace tdx
{
    namespace data
    {
        /**
         * A volume to store and use stack of volumes
         */
        class VolumeStack
        {
            
        public:
            
            /**
             * Constructor with filename of the input stack file name.
             * Format allowed: MRC
             * @param filename
             * @param (int) optional, starting index of the frame (0, 1, 2, ..)
             * @param (int) optional, end index of the frame (0, 1, 2, ..)
             */
            VolumeStack(const std::string& filename, int start=-1, int end=-1);
            
            /**
             * Convert a 3D volume to 2D Volume-Stack
             * @param volume: (Volume2DX) volume to be converted
             * @param (int) optional, starting index of the frame (0, 1, 2, ..)
             * @param (int) optional, end index of the frame (0, 1, 2, ..)
             */
            VolumeStack(Volume2DX volume, int start=-1, int end=-1);
            
            /**
             * Converts the 3D volume to 2D volume stack
             * @param volume
             * @param (int) optional, starting index of the frame (0, 1, 2, ..)
             * @param (int) optional, end index of the frame (0, 1, 2, ..)
             */
            void from_3D_volume(Volume2DX volume, int start=-1, int end=-1);
            
            /**
             * Convert current volume stack to 3D volume
             * @return (Volume2DX) the 3D volume
             */
            Volume2DX to_3D_volume();
            
            /**
             * Return total number of frames in the stack
             * @return (int) total number of frames 
             */
            size_t number_of_frames();
            
            /**
             * Sets the total number of frames
             * @param (int) frames
             */
            void set_number_of_frames(size_t frames);
            
            /**
             * Get the volume header assigned to the stack
             * @return (VolumeHeder)
             */
            VolumeHeader header();
            
            /**
             * Get the data corresponding to the frame number
             * @param (int) frame_number starting from 0, 1, ... , n-1
             * @return (RealSpaceData) data stored at the frame
             */
            RealSpaceData get_frame(int frame_number);
            
            /**
             * Sets the volume at the frame number provided
             * @param (int) frame_number starting from 0, 1, ..., n-1
             * @param (RealSpaceData) data
             */
            void set_frame(int frame, const RealSpaceData& data);
            
        private:
            
            /**
             * A list of all volumes in the stack
             */
            std::vector<RealSpaceData>  _stack;
            
            /**
             * The header data for the volumes
             */
            VolumeHeader _header;
            
        };
    }
}


#endif	/* VOLUME_MOVIE_HPP */

