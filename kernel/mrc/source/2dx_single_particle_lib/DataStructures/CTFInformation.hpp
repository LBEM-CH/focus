#ifndef CTF_INFORMATION_HPP
#define CTF_INFORMATION_HPP

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class CTFParticleInformation;
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
		class CTFParticleInformation : public SingleParticle2dx::DataStructures::InterfaceToString
		{
		public:

			/** Type of the image number */
			typedef SingleParticle2dx::value_type value_type;


			CTFParticleInformation ();


			CTFParticleInformation (value_type defocus, value_type cs, value_type voltage, value_type apix, value_type amp_contrast, value_type bfactor, value_type ast, value_type ast_ang);


			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 *  @post       All memory freed afterwards
			 */
			virtual ~CTFParticleInformation ();


			/**
			 *  @brief      Copy-Constructor
			 *  @details    Makes a deep copy of the passed GlobalParticleInformation
			 *  @param[in]  rhs GlobalParticleInformation to copy
			 */
			CTFParticleInformation (CTFParticleInformation const& rhs);


			/**
			 *  @brief      Assignment operator
			 *  @details    Invokes a deep copy of the passed GlobalParticleInformation
			 *  @param[in]  rhs GlobalParticleInformation to assign
			 */
			CTFParticleInformation& operator= (CTFParticleInformation rhs);


			/**
			 *  @brief      Friend swap function
			 *  @details    Exchanges two GlobalParticleInformations by exchanging the pointers to the 
			 *              members of each GlobalParticleInformation by means of std::swap. This
			 *              technique enables the copy-and-swap trick.
			 *  @param[in]  i1 first GlobalParticleInformation
			 *  @param[in]  i2 second GlobalParticleInformation
			 */
			friend void swap (CTFParticleInformation& i1, CTFParticleInformation& i2);
			
			
			value_type getDefocus() const;
			value_type getCs() const;
			value_type getVoltage() const;
			value_type getApix() const;
			value_type getAmpCon() const;
			value_type getBfacotr() const;
			value_type getAst() const;
			value_type getAngAst() const;
			
			
			void setDefocus(value_type rhs);
			void setCs(value_type rhs);
			void setVoltage(value_type rhs);
			void setApix(value_type rhs);
			void setAmpCon(value_type rhs);
			void setBfactor(value_type rhs);
			void setAst(value_type rhs);
			void setAngAst(value_type rhs);
			
		
			std::string getDataString();
			std::string getDescString();


		private:

			value_type m_defocus;
			value_type m_cs;
			value_type m_voltage;
			value_type m_apix;
			value_type m_amp_contrast;
			value_type m_bfactor;
			value_type m_ast;
			value_type m_ast_ang;
			
			friend class boost::serialization::access;
			   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & m_defocus;
				ar & m_cs;
				ar & m_voltage;
				ar & m_apix;
				ar & m_amp_contrast;
				ar & m_bfactor;
				ar & m_ast;
				ar & m_ast_ang;
			}

		};
		
	} /* DataStructures */
	
} /* SingleParticle2dx */

#endif /* end of include guard: CTF_INFORMATION_HPP */
