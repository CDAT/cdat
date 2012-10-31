set(XML_MAJOR 2)
set(XML_MINOR 7)
set(XML_PATCH 8)
set(XML_MAJOR_SRC 2)
set(XML_MINOR_SRC 7)
set(XML_PATCH_SRC 8)
set(XML_URL ${LLNL_URL})
set(XML_GZ libxml2-${XML_MAJOR_SRC}.${XML_MINOR_SRC}.${XML_PATCH_SRC}.tar.gz)
set(XML_MD5 8127a65e8c3b08856093099b52599c86)

add_cdat_package(libXML2 "" "" "" "")
set(libXML2_deps ${pkgconfig_pkg} ${readline_pkg})

