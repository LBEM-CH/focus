#include "StringFunctions.hpp"



void SingleParticle2dx::Utilities::StringFunctions::splitString(std::vector<std::string>& vec_out, std::string string_in, std::string key)
{
	vec_out.clear();
	std::string splitted_string;
	for ( int i = 0 ; i < static_cast<int>(string_in.length()); i++)
	{
		char tmp_char = string_in[i];
		std::string tmp = std::string(&tmp_char);
		tmp = tmp[0];
		//std::cout << "compare: " << tmp << " " << key << std::endl;
		if(tmp.compare(key) == 0)
		{
			vec_out.push_back(splitted_string);
			splitted_string.clear();
		}
		else
		{
			splitted_string.push_back(string_in[i]);
		}
	}

	if( splitted_string.length() > 0 )
	{
		vec_out.push_back(splitted_string);
	}
}
