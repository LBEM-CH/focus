#ifndef CLASS_INFORMATION_HPP
#define CLASS_INFORMATION_HPP

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class ClassInformation;
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
		class ClassInformation : public SingleParticle2dx::DataStructures::InterfaceToString
		{
		public:

			typedef SingleParticle2dx::size_type size_type;


			ClassInformation ();


			ClassInformation (size_type class_number, size_type true_class = 0);


			virtual ~ClassInformation ();


			ClassInformation (ClassInformation const& rhs);


			ClassInformation& operator= (ClassInformation rhs);

			
			size_type getClass() const;


			void setClass(const size_type rhs);

			
			size_type getTrueClass() const;


			void setTrueClass(const size_type rhs); 


			std::string getDataString();
			
			
			std::string getDescString();


			friend void swap (ClassInformation& i1, ClassInformation& i2);


		private:
			
			size_type m_class_number;
		
			size_type m_true_class_number;

			friend class boost::serialization::access;
			   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & m_class_number; 
				ar & m_true_class_number; 
			}

		};
		
	} /* DataStructures */
	
} /* SingleParticle2dx */

#endif /* end of include guard: CLASS_INFORMATION_HPP */
