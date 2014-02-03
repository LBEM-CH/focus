#include "../2dxSingleParticle.hpp"

int main()
{
	int a = 2;
	float  b = 2.2;
	
	std::cout << "value_int " + SingleParticle2dx::Utilities::StringFunctions::TtoString(a) << std::endl;
	std::cout << "value_float " + SingleParticle2dx::Utilities::StringFunctions::TtoString(b) << std::endl;
	
	
	std::string test_string = "aaa/bbb/ccc/ddd/zzz";
	std::vector<std::string> string_vec;
	
	SingleParticle2dx::Utilities::StringFunctions::splitString(string_vec, test_string, std::string("/"));
	
	for(int i=0; i<static_cast<int>(string_vec.size()); i++)
	{
		std::cout << string_vec[i] << std::endl;
	}
	
	std::cout << "test: " << string_vec.back() << std::endl;
	std::cout << "test: " << string_vec.size() << std::endl;
	std::cout << "test: " << string_vec[string_vec.size()-1] << std::endl;
	
	return 0;
}
