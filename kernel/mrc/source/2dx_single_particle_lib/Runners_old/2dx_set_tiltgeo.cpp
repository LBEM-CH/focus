#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/program_options.hpp>

#include <boost/config/warning_disable.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>
#include <boost/spirit/include/qi_char_.hpp>
#include <boost/spirit/include/qi_string.hpp>
#include <boost/spirit/include/qi_no_skip.hpp>
#include <boost/algorithm/string/predicate.hpp>



int main ()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	std::vector<std::string> image_dirs = config->getImageDirectories();
	
	std::cout << "project dir:" << config->getProjectDirectory() << std::endl;
	
	
	for (SingleParticle2dx::size_type i=1; i<config->getNumberOfImages(); i++)
	{
		std::string working_dir = (config->getProjectDirectory() + image_dirs[i]);
		
		std::vector<std::string> split_vector;
		boost::split( split_vector, working_dir, boost::is_any_of("/") ); 
		std::string filename_core = split_vector.back();
		
		std::string angle_name = filename_core + "/best_orientation.txt";
		
		if ( boost::filesystem::exists(angle_name) )
		{
			std::cout << "file found" << std::endl;
		}
		else
		{
			std::cerr << "angle file (" << angle_name << ") not there" << std::endl;
			throw std::runtime_error("Bad operation");
		}
		
		
		float tltaxis, tltang, taxa;
		std::cout << "parsing " << angle_name << std::endl;
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

			std::string line;
			boost::shared_ptr<std::ifstream> myfile( new std::ifstream(angle_name.c_str()), SingleParticle2dx::Utilities::CustomizedDeleter() );
			
			if (myfile.get()->is_open())
			{
				while ( myfile.get()->good() )
				{
					getline ((*myfile.get()),line);

					if(phrase_parse(
									line.begin(),
									line.end(),

									float_[ref(tltaxis) = _1] >>
									float_[ref(tltang) = _1] >>
									float_[ref(taxa) = _1],

									space)){}
				}
			}
		}
		
		std::cout << "tltaxis: " << tltaxis << std::endl;
		std::cout << "tltang: " << tltang << std::endl;
		std::cout << "taxa: " << taxa << std::endl;
		
		std::string config_in = working_dir + "/2dx_image.cfg";
		std::string config_out = working_dir + "/2dx_image_new.cfg";
		std::string config_backup = working_dir + "/2dx_image_backup.cfg";
		
		if ( !boost::filesystem::exists(config_in) )
		{
			std::cerr << "config_in file (" << config_in << ") not there" << std::endl;
			throw std::runtime_error("Bad operation");
		}
		
		boost::shared_ptr<std::ifstream> myfile_in( new std::ifstream(config_in.c_str()), SingleParticle2dx::Utilities::CustomizedDeleter() );
		boost::shared_ptr<FILE> myfile_out( fopen ( config_out.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
		
		std::string line;
		
		if (myfile_in.get()->is_open())
		{
			while ( myfile_in.get()->good() )
			{
				getline ((*myfile_in.get()),line);
				
				if ( boost::algorithm::starts_with(line, "set TLTAXIS = ") )
				{
					std::cout << "found tltaxis line" << std::endl;
					line = "set TLTAXIS = \"" + boost::lexical_cast<std::string>(tltaxis) + "\"";
				}
				
				if ( boost::algorithm::starts_with(line, "set TLTANG = ") )
				{
					std::cout << "found tltang line" << std::endl;
					line = "set TLTANG = \"" + boost::lexical_cast<std::string>(tltang) + "\"";
				}
				
				if ( boost::algorithm::starts_with(line, "set TAXA = ") )
				{
					std::cout << "found taxa line" << std::endl;
					line = "set TAXA = \"" + boost::lexical_cast<std::string>(taxa) + "\"";
				}
				
				fprintf(myfile_out.get(), "%s\n", line.c_str());
			}
		}
		
		if ( !boost::filesystem::exists(config_out) )
		{
			std::cerr << "config_out file (" << config_out << ") not there" << std::endl;
			throw std::runtime_error("Bad operation");
		}
		
		if (!boost::filesystem::exists(config_backup))
		{
			boost::filesystem::rename(config_in, config_backup);
		}
		
		boost::filesystem::rename(config_out, config_in);
		
		if ( !boost::filesystem::exists(config_in) )
		{
			std::cerr << "final config_in file (" << config_in << ") not there" << std::endl;
			throw std::runtime_error("Bad operation");
		}
		
	}
	
	return 0;
}
