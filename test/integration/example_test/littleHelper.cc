/**
 * @file littleHelper.cc
 * @author Konrad Zemek
 * This is an example C++ helper for integration tests. The contents of all
 * *.cc and *.h files in the test directory are - by convention - all
 * automatically compiled and linked together into a shared library, the name
 * of which depends on the name of the test directory. In this case, the
 * library's filename will be example.so, because the directory name is
 * example_test.
 */

#include <boost/make_shared.hpp>
#include <boost/python.hpp>

using namespace boost::python;

/**
 * This is a simple class whose objects we will use from Python.
 * Boost::Python provides a variety of mapping methods, so we're not limited
 * to a class - e.g. we can also map free functions.
 * Helper C++ code will not always be needed to create integration tests;
 * sometimes it will be enough to directly map the classes we want to test.
 *
 * LittleHelper takes a number in its constructor, stores it, and returns
 * the number on call to @c i() .
 */
class LittleHelper
{
public:
    LittleHelper(int i) : m_i(i) {}
    int i() const { return m_i; }

private:
    int m_i;
};

/**
 * This is a factory method for LittleHelper. A factory method is used here to
 * return @c boost::shared_ptr (@c std::shared_ptr won't work as of the time
 * of writing), so that objects' lifetime is automatically managed in Python.
 */
boost::shared_ptr<LittleHelper> create(int i)
{
    return boost::make_shared<LittleHelper>(i);
}

/**
 * This is the main part of integration tests' C++ files. @c BOOST_PYTHON_MODULE
 * maps C++ code to Python. The module's name and test directory name should be
 * the same - in this case 'example'.
 * The semi-documented feature used here, and frequently throughout integration
 * tests, is the use of @c make_constructor - it simply sets a factory method
 * as a constructor of the mapped class. For details on other mapping features,
 * please consult Boost::Python documentation.
 */
BOOST_PYTHON_MODULE(example)
{
    class_<LittleHelper>("LittleHelper", no_init)
        .def("__init__", make_constructor(create))
        .def("i", &LittleHelper::i);
}
