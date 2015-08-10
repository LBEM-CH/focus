MACRO(GET_PROPAGATE_DEPS _prefix)
        
        get_property(_current DIRECTORY . PROPERTY INCLUDE_DIRECTORIES)
        get_property(_depincs GLOBAL PROPERTY ${_prefix}_DEP_INCLUDES)

        foreach(_dir ${_depincs})
                ##> Filter out duplicates wrt. current-directory properties
                list(FIND _current ${_dir} _isdupl)
                if (_isdupl EQUAL -1)
                        include_directories(${_dir})
                        message("DEBUG> Adding propagated include (${_prefix}->${PROJECT_NAME}): ${_dir}")
                else()
                        message("DEBUG> Omittin duplicate propagated include (${_prefix}->${PROJECT_NAME}): ${_dir}")
                endif()

        endforeach(_dir)


        get_property(_current DIRECTORY . PROPERTY COMPILE_DEFINITIONS)
        get_property(_depdefs GLOBAL PROPERTY ${_prefix}_DEP_DEFINITIONS)

        message(">>>${_current}")
        message(">>>${_depdefs}")
        foreach(_def ${_depdefs})
                ##
                #> Removes duplicate wrt. current-directory  
                list(FIND _current ${_def} _isdupl)
                if (_isdupl EQUAL -1)
                        message("DEBUG> Adding propagated define (${_prefix}->${PROJECT_NAME}): ${_def}")
                        add_definitions(-D${_def})
                else()
                        message("DEBUG> Omitting duplicate propagated define (${_prefix}->${PROJECT_NAME}): ${_def}")
                        
                endif()

        endforeach(_def)        

ENDMACRO(GET_PROPAGATE_DEPS)
