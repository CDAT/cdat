#define CPPUNIT_ASSERT_DOUBLE_ARRAYS_EQUAL( expected,\
             actual,\
             nelements, \
             delta )\
             {\
             for (int tmpTestIndex = 0; tmpTestIndex < nelements; ++tmpTestIndex)\
             {\
             ( CPPUNIT_NS::assertDoubleEquals( (expected[tmpTestIndex]),        \
             (actual[tmpTestIndex]),          \
             (delta),           \
             CPPUNIT_SOURCELINE(), "" ) );\
}\
}

#define CPPUNIT_ASSERT_DOUBLE_2D_ARRAYS_EQUAL( expected,\
                                            actual,\
                                            columns, \
                                            rows,\
                                            delta )\
{\
  for (int tmpTestRow = 0; tmpTestRow < rows; ++tmpTestRow)\
    for (int tmpTestColumn = 0; tmpTestColumn < columns; ++tmpTestColumn)\
{ \
      ( CPPUNIT_NS::assertDoubleEquals( (expected[tmpTestRow][tmpTestColumn]),        \
                                        (actual[tmpTestRow][tmpTestColumn]),          \
                                        (delta),           \
                                        CPPUNIT_SOURCELINE(), "" ) );\
  }\
}
