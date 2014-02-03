#include "../2dxSingleParticle.hpp"

int main (int argc, char const *argv[])
{
	if ( argc != 5 )
	{
		std::cerr << "Usage: ./ShowResults.exe <file_tp_show> <description> <script_name> <importance_flag>" << std::endl;
	}
	else
	{
		std::string filename = argv[1];
		std::string desc = argv[2];
		std::string script = argv[3];
		
		bool importance_flag = false;
		if ( atoi(argv[4]) == 1 )
		{
			importance_flag = true;
		}
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput(filename, desc, script, importance_flag);

	}
	
	return 0;
}
