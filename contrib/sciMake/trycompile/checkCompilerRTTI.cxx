#include <typeinfo>
#include <iostream>
#include <string>

template<class T>
class shared_ptr {
public:
  shared_ptr(T *t) : ptr(t){}

  T *get(){return ptr;}
private:
  T *ptr;
};

struct Base2 {
  virtual void foo(){}
};

struct Derived2 : public Base2 {
  void foo(){}
};

template<class T>
struct Base {
  virtual void foo(){}
};

template<class T>
struct Derived : public Base<T> {
  void foo(){}
};


class Foo{
public:
  bool checkType(shared_ptr<Base<double> > &s){
    return std::string(typeid(*s.get()).name()) == std::string(typeid(Derived<double>).name());
  }
};

class Foo2{
public:
  bool checkType(shared_ptr<Base2> &s){
    return std::string(typeid(*s.get()).name()) == std::string(typeid(Derived2).name());
  }
};

template<class T>
bool testTemplate(){
  shared_ptr<Base<double> > b(new T());
  Foo f;
  return f.checkType(b);
}

template<class T>
bool testNonTemplate(){
  shared_ptr<Base2> b(new T());
  Foo2 f;
  return f.checkType(b);
}

int main(){
  // per convention, return of 0 is pass, 1 is fail. So in the end we need to
  // take the compliment of the flag value.
  bool flag = true;

  shared_ptr<Base<double> > shared(new Derived<double>());
  Base<double> *base = new Derived<double>();

  flag = (typeid(*base) == typeid(Derived<double>)) && (typeid(*shared.get()) == typeid(Derived<double>));
  flag = flag && testNonTemplate<Derived2>();
  flag = flag && testTemplate<Derived<double> >();

  return !flag;
}
