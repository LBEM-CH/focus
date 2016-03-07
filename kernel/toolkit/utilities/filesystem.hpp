/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef FILESYSTEM_HPP
#define	FILESYSTEM_HPP

#include <iostream>
#include <string>

namespace tdx
{
    namespace utilities
    {
        namespace filesystem
        {
            /**
             * A method to check if a file exists in the path
             * @param file_path
             * @return True if file exists, False otherwise
             */
            bool FileExists(const std::string file_path);
            
            /**
             * Returns the file extension.
             * Evaluates the letters after the final '.' appearing in the string.
             * If '.' is absent returns ''
             * @param file_path
             * @return the extension of the file
             */
            std::string FileExtension(const std::string file_path);
            
            /**
             * Returns the number of columns in a file.
             * Also considers if there are irregular heading lines present 
             * @param file_name : String path of file
             * @param number_columns : Returns the number of columns in this variable
             * @return number of header lines if found
             */
            int NumberOfColumns(const std::string file_name, int& number_columns);
            
        }
        
    }
    
}


#endif	/* FILESYSTEM_HPP */

