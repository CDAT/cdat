# create an external project to clone vistrails,
# and configure and build it

include(GetGitRevisionDescription)
set(vistrails_branch uvcdat-master)

get_git_head_revision(refspec sha)
if("${refspec}" STREQUAL "refs/heads/next")
  set(vistrails_branch uvcdat-next)
endif()

set(VISTRAILS_TAG_POINT ${vistrails_branch} CACHE STRING "Specify branch of vistrails to be used for UVCDAT")
set(vistrails_url "${GIT_PROTOCOL}vistrails.org/git/vistrails.git")

option(CDAT_DELETE_VISTRAILS_HISTORY "Delete GIT history of vistrails" OFF)
option(CDAT_AUTO_UPDATE_VISTRAILS_TAG_POINT "Delete GIT history of vistrails" ON)

# FIXME: Workaround. For some reason just using GIT_* to clone breaks the superbuild.
set(vistrails_install_command ${GIT_EXECUTABLE} clone --depth 1 -b ${VISTRAILS_TAG_POINT}  ${vistrails_url})
if(EXISTS ${CMAKE_INSTALL_PREFIX}/vistrails/.git)
  if(CDAT_AUTO_UPDATE_VISTRAILS_TAG_POINT)
    set(VISTRAILS_TAG_POINT ${vistrails_branch} CACHE STRING "" FORCE)
  endif()
  execute_process(
    COMMAND ${GIT_EXECUTABLE} checkout -f ${VISTRAILS_TAG_POINT}
    WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/vistrails
  )

  set(vistrails_install_command ${GIT_EXECUTABLE} --git-dir=${CMAKE_INSTALL_PREFIX}/vistrails/.git pull)
endif()

ExternalProject_Add(vistrails
  DOWNLOAD_DIR ${CMAKE_CURRENT_BINARY_DIR}
  SOURCE_DIR ${CMAKE_INSTALL_PREFIX}
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ${vistrails_install_command}
  DEPENDS ${vistrails_DEPENDENCIES}
  ${EP_LOG_OPTIONS}
)

if(CDAT_DELETE_VISTRAILS_HISTORY)
  ExternalProject_Add_Step(vistrails after_install
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_INSTALL_PREFIX}/vistrails/.git
    DEPENDEES install
    WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/vistrails
  )
endif()
