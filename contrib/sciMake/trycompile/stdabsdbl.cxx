/**
 * $Id: stdabsdbl.cxx 1158 2011-12-17 14:18:42Z cary $
 *
 * Determine whether the compiler knows std::abs<double>.
 */

#include <cmath>

int main(int argc, char** argv) {
  double a = 0;
  double b = std::abs(a);
  return 0;
}

