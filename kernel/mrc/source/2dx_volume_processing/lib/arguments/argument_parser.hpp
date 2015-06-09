/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef ARGUMENT_PARSER_HPP
#define	ARGUMENT_PARSER_HPP

#include <iostream>
#include <vector>
#include <string>
#include <boost/program_options.hpp>

#include "argument_template.hpp"

namespace po = boost::program_options;

namespace volume
{
    namespace arguments
    {
        /**
         * A class to parse command line arguments. 
         */
        class ArgumentParser
        {
            
        public:
            /**
             * Constructor parsing the command line arguments. 
             * @param arguments: A vector of Arguments (Can be constructed or used from template)
             * @param ac : arguments count
             * @param av : char* to all arguments
             * @param prog_help : A small string help of the program 
             */
            ArgumentParser(std::vector<Argument> arguments, int ac, char* av[], const std::string& prog_help="")
            {
                const char* prog_help_inp = (prog_help+"\nProgram Options").c_str();
                po::options_description opts_description(prog_help_inp);

                //Add help
                opts_description.add_options()
                    ("help", "produce help message");
                ;

                for(std::vector<Argument>::const_iterator itr=arguments.begin(); itr!=arguments.end(); ++itr)
                {
                    Argument arg = (*itr);
                    const char* name;
                    if(arg.identifier() != "") name = (arg.name() + "," + arg.identifier()).c_str();
                    else name = (arg.name()).c_str();

                    const char* help = (arg.help()).c_str();
                    if(arg.has_arg())
                    {   
                        opts_description.add_options()
                            (name, po::value<std::string>(), help);
                        ;
                    }
                    else
                    {
                       opts_description.add_options()
                            (name, help);
                        ; 
                    }
                }

                description.add(opts_description);

                try
                {
                    po::store(po::parse_command_line(ac, av, opts_description), variables);
                }
                catch(std::exception& e)
                {
                    std::cerr << "Error while parsing command line options.\n";
                    std::cerr << e.what() << "\n\n\n";
                    std::cerr << "See below for more details on program's description and input.\n";
                    std::cerr << description << "\n";
                    exit(1);
                }
                po::notify(variables);

                //produce help if specified
                if(variables.count("help"))
                {
                    std::cout << description << "\n";
                    exit(0);
                }
            };
            
            /**
             * Returns the value of the argument in string format. If the argument is
             * not present return blank string.
             * @param arg
             * @param required - Is the argument be made compulsory? 
             * @return 
             */
            std::string get(Argument arg, bool required=false)
            {

                if(required && !(variables.count(arg.name())) )
                {

                    std::cerr << "\n::ERROR: " << arg.name() << " is required and was not found!!\n";
                    std::cerr << "See below for more details on program's description and input.\n";
                    std::cerr << description << "\n";
                    exit(1);
                }
                else if(variables.count(arg.name()))
                {
                    if(arg.has_arg())
                    {
                        std::cout << ":: " << arg.name().c_str() << " = " << variables[arg.name()].as<std::string>() <<"\n";
                    }
                    else
                    {
                        std::cout << ":: " << arg.name().c_str() << "\n";
                    }

                    return variables[arg.name()].as<std::string>();
                }
                else
                {
                    return "";
                }
            };
            
            /**
             * Returns true if the argument is present, and false if it is not present.
             * @param arg
             * @param required
             * @return int value of the argument
             */
            bool get_bool(Argument arg, bool required=false)
            {
                if(required && !(variables.count(arg.name())) )
                {

                    std::cerr << "\n::ERROR: " << arg.name() << " is required and was not found!!\n";
                    std::cerr << "See below for more details on program's description and input.\n";
                    std::cerr << description << "\n";
                    exit(1);
                }
                else if(variables.count(arg.name()))
                {
                    std::cout << ":: " << arg.name().c_str() << "\n";
                    return true;
                }
                else
                {
                    return false;
                }    
            };
            
            /**
             * Returns the int value of the option. If not present returns 0.
             * Use this method if the option has an argument which is expected to be int.
             * @param arg
             * @param required
             * @return 
             */
            int get_int(Argument arg, bool required=false)
            {
                std::string value = get(arg, required);
                if(value != "")
                {
                    try{
                        return std::stoi(value.c_str());
                    }
                    catch(std::exception& e) 
                    {
                        std::cerr << "\n::ERROR while parsing " << arg.name() << "\n";
                        std::cerr << "::Please check the value!!\n";
                        exit(1);
                    }
                }
                else
                {
                    return 0;
                }
            };
            
            /**
             * Returns the double value of the option. If not present returns 0.
             * Use this method if the option has an argument which is expected to be double.
             * @param arg
             * @param required
             * @return 
             */
            double get_double(Argument arg, bool required=false)
            {
                std::string value = get(arg, required);
                if(value != "")
                {
                    try{
                        return std::stof(value.c_str());
                    }
                    catch(std::exception& e) 
                    {
                        std::cerr << "\n::ERROR while parsing " << arg.name() << "\n";
                        std::cerr << "::Please check the value!!\n";
                        exit(1);
                    }
                }
                else
                {
                    return 0.0;
                }
            };
            
            friend inline std::ostream& operator<<(std::ostream& os, const ArgumentParser& parser)
            {
                os << parser.description<<"\n";
                return os;
            }
            
        private:
            po::variables_map variables;
            po::options_description description;
            
        };
        
    }
}

#endif	/* ARGUMENT_PARSER_HPP */

