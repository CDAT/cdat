set(XML_MAJOR 2)
set(XML_MINOR 7)
set(XML_PATCH 8)
set(XML_MAJOR_SRC 2)
set(XML_MINOR_SRC 7)
set(XML_PATCH_SRC 8)
set(XML_URL ${LLNL_URL})
set(XML_GZ libxml2-${XML_MAJOR_SRC}.${XML_MINOR_SRC}.${XML_PATCH_SRC}.tar.gz)
set(XML_MD5 8127a65e8c3b08856093099b52599c86)

set (nm XML)
string(TOUPPER ${nm} uc_nm)
set(${uc_nm}_VERSION ${${nm}_MAJOR_SRC}.${${nm}_MINOR_SRC}.${${nm}_PATCH_SRC})
set(LIBXML2_VERSION ${XML_VERSION})
set(LIBXML2_SOURCE ${XML_URL}/${XML_GZ} )
set(LIBXML2_MD5 ${XML_MD5})

add_cdat_package(libXML2 "" "Bulid libxml2" "")

