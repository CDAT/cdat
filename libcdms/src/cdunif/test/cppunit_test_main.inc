int main( int ac, char **av )
{
  CppUnit::Test *suite = CppUnit::TestFactoryRegistry::getRegistry().makeTest();
  CppUnit::TextUi::TestRunner runner;
  runner.addTest( suite );
  runner.setOutputter( new CppUnit::CompilerOutputter( &runner.result(),
                       std::cerr ) );
  bool wasSucessful = runner.run();
  return wasSucessful ? 0 : 1;
}
