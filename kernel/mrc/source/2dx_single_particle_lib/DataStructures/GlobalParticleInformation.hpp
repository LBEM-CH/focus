#ifndef GLOBAL_PARTICLE_INFORMATION_HPP
#define GLOBAL_PARTICLE_INFORMATION_HPP

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class GlobalParticleInformation;
	}
}

#include <vector>

#include <boost/serialization/utility.hpp>

#include "../Typedefs.hpp"
#include "InterfaceToString.hpp"


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		
		/**
		 *  @brief     Container storing the global information of an individual particle
		 *  @details   For several reasons (debug, stat, ...) we store the global information of each 
		               particle. Based on the image number and the position of the picked particle we 
		               can easily reconstruct from where we picked the particle originally.
		 *  @author    Sebastian Scherer
		 *  @version   0.2
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class GlobalParticleInformation : public SingleParticle2dx::DataStructures::InterfaceToString
		{
		public:

			/** Type of the image number */
			typedef SingleParticle2dx::size_type number_type;


			/** Type of the particle position in one spacial direction */
			typedef SingleParticle2dx::size_type position_type;


			/** Type of the entire particle position */
			typedef std::pair<position_type,position_type> position_array_type;


			/**
			 *  @brief      Default Constructor
			 *  @details    Initalizes all internal values to zero
			 *  @post       Image number and particle position set to zero
			 */
			GlobalParticleInformation ();


			/** 
			 *  @brief      Constructor
			 *  @details    Costum Constructor which initializes the GlobalParticleInformation with 
			 *              given values
			 *  @param[in]  nr Image number
			 *  @param[in]  x1 Position of the particle in x-direction
			 *  @param[in]  x2 Position of the particle in y-direction
			 *  @post       GlobalParticleInformation set to the given values
			 */
			GlobalParticleInformation (number_type nr, position_type x1, position_type x2);


			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 *  @post       All memory freed afterwards
			 */
			virtual ~GlobalParticleInformation ();


			/**
			 *  @brief      Copy-Constructor
			 *  @details    Makes a deep copy of the passed GlobalParticleInformation
			 *  @param[in]  rhs GlobalParticleInformation to copy
			 */
			GlobalParticleInformation (GlobalParticleInformation const& rhs);


			/**
			 *  @brief      Assignment operator
			 *  @details    Invokes a deep copy of the passed GlobalParticleInformation
			 *  @param[in]  rhs GlobalParticleInformation to assign
			 */
			GlobalParticleInformation& operator= (GlobalParticleInformation rhs);


			/**
			 *  @brief      Friend swap function
			 *  @details    Exchanges two GlobalParticleInformations by exchanging the pointers to the 
			 *              members of each GlobalParticleInformation by means of std::swap. This
			 *              technique enables the copy-and-swap trick.
			 *  @param[in]  i1 first GlobalParticleInformation
			 *  @param[in]  i2 second GlobalParticleInformation
			 */
			friend void swap (GlobalParticleInformation& i1, GlobalParticleInformation& i2);


			/**
			 *  @brief      Returns the image number
			 *  @details    Trivial get-Operation
			 *  @return     Image number
			 */
			number_type getImageNumber () const;


			/**
			 *  @brief      Sets the image number to a given value
			 *  @details    Trivial set-Operation
			 *  @param[in]  nr value to set
			 */
			void setImageNumber (const number_type nr);


			/**
			 *  @brief      Return the position in x-direction of the particle
			 *  @details    Trivial get-Operation
			 *  @return     Value of the position in x direction of the particle
			 */
			position_type getPositionX () const;


			/**
			 *  @brief      Sets the value of the particle position in x direction
			 *  @details    Trivial set-Operation
			 *  @param[in]  pos value to set
			 */
			void setPositionX (const position_type pos);


			/**
			 *  @brief      Returns the position in y-direction of the particle
			 *  @details    Trivial get-Operation
			 *  @return     Value of the position in y direction of the particle
			 */
			position_type getPositionY () const;


			/**
			 *  @brief      Sets the value of the particle position in y direction
			 *  @details    Trivial set-Operation
			 *  @param[in]  pos value to set
			 */
			void setPositionY (const position_type pos);
			
			
			std::string getDataString();
			
			
			std::string getDescString();
			
			
			void setImageWidth(size_type rhs);
			
			
			size_type getImageWidth();
			
			void setImageName(std::string rhs);
			std::string getImageName();
			

		private:

			/** @brief Image number of the image from which the particle was picked */
			number_type m_image_number;

			/** @brief Pixel position of the center of the picked paricle */
			position_array_type m_position;
			
			size_type m_image_width;
			
			friend class boost::serialization::access;
			
			std::string m_image_name;
			   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & m_image_number;
				ar & m_position;
				ar & m_image_width;
				ar & m_image_name;
			}

		};
		
	} /* DataStructures */
	
} /* SingleParticle2dx */

#endif /* end of include guard: GLOBAL_PARTICLE_INFORMATION_HPP */
