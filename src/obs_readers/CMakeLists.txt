# Set the name of the plugins in this directory that should be added to the build.
# For each plugin, the following rules must be followed
#   1) file   name is "plugin".F90
#   2) module name is "plugin"_mod
#   3) class  name is "plugin"
#-------------------------------------------------------------------------------
set(PLUGINS
  obs_reader_nc
  obs_reader_wod
)

if(OBSQC_USE_BUFR)
  list(APPEND PLUGINS obs_reader_bufr)
endif()



#-------------------------------------------------------------------------------
# Don't edit below here.
#-------------------------------------------------------------------------------
# This automatically generates a source code file "PLUGIN_LOADER_FILE" that will
# be added to the build with the purpose of registering each plugin with the
# main program.
#-------------------------------------------------------------------------------
set(PLUGIN_LOADER_FILE ${CMAKE_CURRENT_BINARY_DIR}/obs_reader_pluginloader.F90)

# for each plugin listed above, add its source to the build, add to the list of
# modules to "use", add to list of classes to register
set(PLUGIN_MODS "")
set(PLUGIN_ALLOCS "")
foreach(p ${PLUGINS})
  target_sources(obsqc PRIVATE ${CMAKE_CURRENT_LIST_DIR}/${p}.F90)
  STRING(CONCAT PLUGIN_MODS ${PLUGIN_MODS}
    "USE ${p}_mod\n")
  STRING(CONCAT PLUGIN_ALLOCS ${PLUGIN_ALLOCS}
    "ALLOCATE(${p}::ptr)\n"
    "wrapper%p=>ptr\n"
    "CALL obs_readers%push_back(wrapper)\n" )
endforeach()

# generate the source file, and add it to the build
configure_file(plugin_loader.F90.in ${PLUGIN_LOADER_FILE})
target_sources(obsqc PRIVATE ${PLUGIN_LOADER_FILE})
