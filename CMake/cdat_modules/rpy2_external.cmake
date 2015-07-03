# create an external project to install RPY2,
# and configure and build it
set(nm RPY2)

# Set PATH and R_HOME to find R
list(APPEND USR_ENVS
  "R_HOME=${cdat_EXTERNALS}/lib/R"
  "PATH=${cdat_EXTERNALS}/bin:$ENV{PATH}"
  )

include(pipinstaller)
