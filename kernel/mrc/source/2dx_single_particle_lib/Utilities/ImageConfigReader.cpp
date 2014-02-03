#include <iostream>
#include <fstream>

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/qi_char_.hpp>
#include <boost/spirit/include/qi_string.hpp>

#include "ImageConfigReader.hpp"
#include "../Utilities.hpp"


SingleParticle2dx::Utilities::ImageConfigReader::ImageConfigReader (std::string filename)
{
	using boost::spirit::qi::float_;
	using boost::spirit::qi::char_;
	using boost::spirit::qi::_1;
	using boost::spirit::qi::phrase_parse;
	using boost::spirit::ascii::space;
	using boost::spirit::ascii::string;
	using boost::phoenix::ref;
	using boost::phoenix::push_back;
	
	boost::shared_ptr<std::ifstream> myfile( new std::ifstream(filename.c_str()), SingleParticle2dx::Utilities::CustomizedDeleter() );
	std::string line;
	
	if (myfile.get()->is_open())
	{
		while ( myfile.get()->good() )
		{
			getline ((*myfile.get()),line);
						
			std::string name;
			std::vector<value_type> vec;
			
			if(phrase_parse(line.begin(),
							line.end(),
							
							string("set") >>
							(+char_("a-zA-Z_")[boost::phoenix::ref(name)+=_1]) >>
							"=" >>
							'"' >>
							+((+float_[push_back(boost::phoenix::ref(vec), _1)]) >> (*char_(","))),
							
							space))
			{
				m_data[name] = vec;
			}	
		}
	}
}


SingleParticle2dx::Utilities::ImageConfigReader::~ImageConfigReader ()
{
	
}


void SingleParticle2dx::Utilities::ImageConfigReader::printImageConfigFile()
{
	std::map<std::string, std::vector<value_type> >::iterator map_it;
	std::vector<value_type>::iterator vec_it;

	std::cout << std::endl << "Image Variables\n";
	for ( map_it=m_data.begin() ; map_it != m_data.end(); map_it++ )
	{
		std::cout << (*map_it).first << ":\n\t";
		for ( vec_it=(*map_it).second.begin() ; vec_it != (*map_it).second.end(); vec_it++ )
		{
			std::cout << (*vec_it) << " ";
		}
		std::cout << std::endl << std::endl;
	}
}


std::vector<SingleParticle2dx::Utilities::ImageConfigReader::value_type> SingleParticle2dx::Utilities::ImageConfigReader::getConfigElement (std::string key)
{
	std::vector<value_type> result = m_data[key];
	if (result.size() == 0)
	{
		SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("WARNING - unknown parameter read from config file: " + key, 1);
		result.push_back(-9999);
	}
	return result;
}
