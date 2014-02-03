#include <iostream>
#include <fstream>

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/qi_char_.hpp>
#include <boost/spirit/include/qi_string.hpp>

#include "MergeVariableReader.hpp"

#include "../Utilities/CustomizedDeleter.hpp"



SingleParticle2dx::Utilities::MergeVariableReader::MergeVariableReader (std::string filename)
{
	using boost::spirit::qi::float_;
	using boost::spirit::qi::char_;
	using boost::spirit::qi::_1;
	using boost::spirit::qi::lit;
	using boost::spirit::qi::phrase_parse;
	using boost::spirit::ascii::space;
	using boost::spirit::ascii::string;
	using boost::phoenix::ref;
	using boost::phoenix::push_back;
	using boost::spirit::no_skip;
	
		
	boost::shared_ptr<std::ifstream> myfile( new std::ifstream(filename.c_str()), SingleParticle2dx::Utilities::CustomizedDeleter() );
	std::string line;
	
	if (myfile.get()->is_open())
	{
		while ( myfile.get()->good() )
		{
			getline ((*myfile.get()),line);
			line += " ";
			
			std::string name_float;
			std::string name_string;
			std::string name_tmp;
			
			std::vector<value_type> vec_float;
			std::vector<std::string> vec_string;
			
			if(phrase_parse(
							line.begin(),
							line.end(),
							
							no_skip[
								(+char_("0-9a-zA-Z_")[boost::phoenix::ref(name_float)+=_1]) >>
								(+space) >> 
								+((+float_[boost::phoenix::push_back(boost::phoenix::ref(vec_float), _1)]) >> (*char_(",")))
							],
							
							space))
			{
				m_float_data[name_float] = vec_float;
			}
			else if(phrase_parse(
							line.begin(),
							line.end(),
							
							no_skip[
								(+char_("0-9a-zA-Z_-")[boost::phoenix::ref(name_string)+=_1]) >>
								(+space) >>
								+( char_[boost::phoenix::ref(name_tmp)=_1] >> (+char_("0-9a-zA-Z_/-")[boost::phoenix::ref(name_tmp)+=_1]) >> (+space))[boost::phoenix::push_back(boost::phoenix::ref(vec_string), boost::phoenix::ref(name_tmp))]
							],
							
							space))
			{
				for ( std::vector<std::string>::iterator it = vec_string.begin(); it != vec_string.end(); it++)
				{
					m_string_data[name_string].push_back(*it);
				}
			}
		}
	}
}


SingleParticle2dx::Utilities::MergeVariableReader::~MergeVariableReader ()
{
	
}


std::vector<SingleParticle2dx::Utilities::MergeVariableReader::value_type> SingleParticle2dx::Utilities::MergeVariableReader::getFloatElement (std::string key)
{
	std::vector<value_type> result = m_float_data[key];
	if (result.size() == 0)
	{
		//SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("WARNING - unknown parameter read from config file: " + key, 1);
		result.push_back(-9999);
		result.push_back(-9999);
		result.push_back(-9999);
		result.push_back(-9999);
	}
	return result;
}


std::vector<std::string> SingleParticle2dx::Utilities::MergeVariableReader::getStringElement (std::string key)
{
	std::vector<std::string> result = m_string_data[key];
	if (result.size() == 0)
	{
		//SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("WARNING - unknown parameter read from config file: " + key, 1);
		result.push_back("invalid");
	}
	return result;
}


void SingleParticle2dx::Utilities::MergeVariableReader::printConfigFile()
{
	std::map<std::string, std::vector<value_type> >::iterator map_float_it;
	std::map<std::string, std::vector<std::string> >::iterator map_string_it;
	std::vector<value_type>::iterator vec_it;
	std::vector<std::string>::iterator string_it;

	std::cout << std::endl << "Merge Variables:\n";
	for ( map_float_it=m_float_data.begin() ; map_float_it != m_float_data.end(); map_float_it++ )
	{
		std::cout << (*map_float_it).first << ":\n\t";
		for ( vec_it=(*map_float_it).second.begin() ; vec_it != (*map_float_it).second.end(); vec_it++ )
		{
			std::cout << (*vec_it) << " ";
		}
		std::cout << std::endl << std::endl;
	}
	
	for ( map_string_it=m_string_data.begin() ; map_string_it != m_string_data.end(); map_string_it++ )
	{
		std::cout << (*map_string_it).first << ":\n\t";
		for ( string_it=(*map_string_it).second.begin() ; string_it != (*map_string_it).second.end(); string_it++ )
		{
			std::cout << (*string_it) << " ";
		}
		std::cout << std::endl << std::endl;
	}
}
