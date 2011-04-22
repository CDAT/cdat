/* Test cdunif/GRIB */

#include <cstdio>
#include <cstring>
#include <cmath>
#include <cstdlib>

#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/ui/text/TestRunner.h>

#include "cppunit_array_helper.h"
extern "C"
{
  #include "cdunif.h"
}

#include "testgrib-data.h"

#define EPSILON 1.0e-3

class TestGrib : public CPPUNIT_NS::TestCase
{
  CPPUNIT_TEST_SUITE(TestGrib);
    CPPUNIT_TEST(testOpenGRIB1File);
    CPPUNIT_TEST(testGetGRIB1VariableAttributeTitle);
    CPPUNIT_TEST(testGetGRIB1Latitudes);
    CPPUNIT_TEST(testGetGRIB1GridData);
#ifdef GRIB2
    CPPUNIT_TEST(testOpenGRIB2File);
    CPPUNIT_TEST(testGetGRIB2VariableAttributeTitle);
    CPPUNIT_TEST(testGetGRIB2Latitudes);
    CPPUNIT_TEST(testGetGRIB2Latitudes_2);
    CPPUNIT_TEST(testGetGRIB2GridData);
#endif    
  CPPUNIT_TEST_SUITE_END();

  public:
    void setUp(void) {}
    void tearDown(void) {}

    protected:
      void testOpenGRIB1File(void)
      {
        int fileId = cuopenread("testgrib.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);
      }


      void testOpenGRIB2File(void)
      {
        int fileId = cuopenread("testgrib2.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);
      }


      void testGetGRIB1VariableAttributeTitle(void)
      {
        int fileId = cuopenread("testgrib.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);

        int variableId = cuvarid(fileId, "hgtsfc");
        CPPUNIT_ASSERT(variableId > -1);

        int attributeLength = -1;
        std::string expectedTitle = std::string("** surface Geopotential height [gpm]");

        cuattinq(fileId, variableId, "title", 0, &attributeLength);
        CPPUNIT_ASSERT_EQUAL(expectedTitle.length() + 1, size_t(attributeLength));

        char title[CU_MAX_NAME];
        cuattget(fileId, variableId, "title", title);
        CPPUNIT_ASSERT_EQUAL(expectedTitle, std::string(title));

        float missing_value = 0;
        cuattget(fileId, variableId, "missing_value", &missing_value);
        CPPUNIT_ASSERT_DOUBLES_EQUAL((float)9.999e20, missing_value, 1e-10);
      }


      void testGetGRIB2VariableAttributeTitle(void)
      {
        int fileId = cuopenread("testgrib2.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);

        int variableId = cuvarid(fileId, "swdir_1");
        CPPUNIT_ASSERT(variableId > -1);

        int attributeLength = -1;
        std::string expectedTitle = std::string("** 1 in sequence Direction of Swell Waves [deg]");

        cuattinq(fileId, variableId, "title", 0, &attributeLength);
        CPPUNIT_ASSERT_EQUAL(expectedTitle.length() + 1, size_t(attributeLength));

        char title[CU_MAX_NAME];
        cuattget(fileId, variableId, "title", title);
        CPPUNIT_ASSERT_EQUAL(expectedTitle, std::string(title));
      }


      void testGetGRIB1Latitudes(void)
      {
        int fileId = cuopenread("testgrib.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);

        int latitudeDimensionId = cudimid(fileId, CU_GLOBAL, "latitude");
        CPPUNIT_ASSERT(latitudeDimensionId > -1 );

        float latitudesRead[GRIB1_NLAT];
        cudimget(fileId, latitudeDimensionId, latitudesRead);
        CPPUNIT_ASSERT(latitudesRead != 0);
        CPPUNIT_ASSERT_DOUBLE_ARRAYS_EQUAL(GRIB1_TEST_DATA_LATITUDES, latitudesRead, GRIB1_NLAT, EPSILON);
      }


      void testGetGRIB2Latitudes(void)
      {
        int fileId = cuopenread("testgrib2.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);

        int latitudeDimensionId = cudimid(fileId, CU_GLOBAL, "latitude");
        CPPUNIT_ASSERT(latitudeDimensionId > -1 );

        float latitudesRead[GRIB2_NLAT];
        cudimget(fileId, latitudeDimensionId, latitudesRead);
        CPPUNIT_ASSERT(latitudesRead != 0);
        CPPUNIT_ASSERT_DOUBLE_ARRAYS_EQUAL(GRIB2_TEST_DATA_LATITUDES, latitudesRead, GRIB2_NLAT, EPSILON);
      }

      void testGetGRIB2Latitudes_2(void)
      {
        int fileId = cuopenread("gfs20100916/gfs_master_12z.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);

        int latitudeDimensionId = cudimid(fileId, CU_GLOBAL, "latitude");
        CPPUNIT_ASSERT(latitudeDimensionId > -1 );

        float latitudesRead[GRIB2_2_NLAT];
        cudimget(fileId, latitudeDimensionId, latitudesRead);
        CPPUNIT_ASSERT(latitudesRead != 0);
        CPPUNIT_ASSERT_DOUBLE_ARRAYS_EQUAL(GRIB2_2_TEST_DATA_LATITUDES, latitudesRead, GRIB2_NLAT, EPSILON);
      }


      void testGetGRIB1GridData(void)
      {
        int fileId = cuopenread("testgrib.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);

        int variableId = cuvarid(fileId, "tmpsfc");
        CPPUNIT_ASSERT(variableId > -1 );

        long start[4];
        long count[4];
        start[0]=0;
        start[1]=0;
        start[2]=0;

        count[0]=1;
        count[1]=GRIB1_NLAT;
        count[2]=GRIB1_NLON;
        float dataRead[GRIB1_NLAT][GRIB1_NLON];

        cuvarget(fileId, variableId, start, count, dataRead);
        CPPUNIT_ASSERT_DOUBLE_2D_ARRAYS_EQUAL(GRIB1_TEST_DATA_T0, dataRead, GRIB1_NLON, GRIB1_NLAT, EPSILON);

        start[0]=1;
        cuvarget(fileId, variableId, start, count, dataRead);
        CPPUNIT_ASSERT_DOUBLE_2D_ARRAYS_EQUAL(GRIB1_TEST_DATA_T1, dataRead, GRIB1_NLON, GRIB1_NLAT, EPSILON);

        start[0]=0;
        cuvarget(fileId, variableId, start, count, dataRead);
        CPPUNIT_ASSERT_DOUBLE_2D_ARRAYS_EQUAL(GRIB1_TEST_DATA_T0, dataRead, GRIB1_NLON, GRIB1_NLAT, EPSILON);
      }

      void testGetGRIB2GridData(void)
      {
        int fileId = cuopenread("testgrib2.ctl", 0);
        CPPUNIT_ASSERT(fileId > -1);

        int variableId = cuvarid(fileId, "swell_1");
        CPPUNIT_ASSERT(variableId > -1 );

        long start[4];
        long count[4];
        start[1]=0;
        start[2]=0;

        count[0]=1;
        count[1]=GRIB2_NLAT;
        count[2]=GRIB2_NLON;
        float dataRead[GRIB2_NLAT][GRIB2_NLON];

        start[0]=13;
        cuvarget(fileId, variableId, start, count, dataRead);
        CPPUNIT_ASSERT_DOUBLE_2D_ARRAYS_EQUAL(GRIB2_TEST_DATA_T0, dataRead, GRIB2_NLON, GRIB2_NLAT, EPSILON);

        start[0]=14;
        cuvarget(fileId, variableId, start, count, dataRead);
        CPPUNIT_ASSERT_DOUBLE_2D_ARRAYS_EQUAL(GRIB2_TEST_DATA_T1, dataRead, GRIB2_NLON, GRIB2_NLAT, EPSILON);

        start[0]=14;
        cuvarget(fileId, variableId, start, count, dataRead);
        CPPUNIT_ASSERT_DOUBLE_2D_ARRAYS_EQUAL(GRIB2_TEST_DATA_T1, dataRead, GRIB2_NLON, GRIB2_NLAT, EPSILON);
      }

};

CPPUNIT_TEST_SUITE_REGISTRATION(TestGrib);

#include "cppunit_test_main.inc"
