/**
 * $Id: gendeclstatics.cxx 1158 2011-12-17 14:18:42Z cary $
 */

template <class TYPE> class X {
  public:
    static int r;
};
template <class TYPE> int X<TYPE>::r = 0;

int main (int argc, char* argv[]) {
  X<double> x;
  int rr = x.r + X<float>::r;
};

