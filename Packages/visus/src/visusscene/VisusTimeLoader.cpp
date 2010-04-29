#include "VisusTimeLoader.h"

int (* VisusTimeLoader::callback) (const VisusTime&) = NULL;

int VisusTimeLoader::loadTimeStep(const VisusTime& current_time,
                                  std::vector<unsigned char*>& data)
{
  return 1;
}
