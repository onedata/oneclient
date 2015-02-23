#include <boost/make_shared.hpp>
#include <boost/python.hpp>

using namespace boost::python;

class LittleHelper
{
public:
    LittleHelper(int i) : m_i(i) {}
    int i() const { return m_i; }

private:
    int m_i;
};

boost::shared_ptr<LittleHelper> create(int i)
{
    return boost::make_shared<LittleHelper>(i);
}

BOOST_PYTHON_MODULE(example)
{
    class_<LittleHelper>("LittleHelper", no_init)
        .def("__init__", make_constructor(create))
        .def("i", &LittleHelper::i);
}
