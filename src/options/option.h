/**
 * @file option.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_OPTION_H
#define ONECLIENT_OPTION_H

#include <boost/optional.hpp>
#include <boost/program_options.hpp>

#include <sstream>

namespace one {
namespace client {
namespace options {

enum class OptionGroup { GENERAL, FUSE, INVISIBLE, DEPRECATED };

/**
 * @c Event class represents an abstract client option. It provides an interface
 * for concrete options.
 */
class Option {
public:
    virtual ~Option() = default;

    /**
     * Checks whether option belongs to specified group.
     * @param group A group to be checked.
     * @return true if option belongs to the specified group, otherwise false.
     */
    virtual bool hasGroup(const OptionGroup &group) const = 0;

    /**
     * Checks whether option is present in variables map.
     * @param vm A variables map.
     * @return true is option is present in provided variables map, otherwise
     * false.
     */
    virtual bool isSet(
        const boost::program_options::variables_map &vm) const = 0;

    /**
     * @return name of the option.
     */
    virtual std::string name() const = 0;

    /**
     * Adds options to the command line options description.
     * @param A command line options description.
     */
    virtual void addCommandLine(
        boost::program_options::options_description &desc) = 0;

    /**
     * Adds options to the positional options description.
     * @param A positional options description.
     */
    virtual void addPositional(
        boost::program_options::positional_options_description &desc) = 0;

    /**
     * Adds options to the environment options description.
     * @param An environment options description.
     */
    virtual void addEnvironment(
        boost::program_options::options_description &desc) = 0;

    /**
     * Adds options to the configuration file options description.
     * @param A configuration file options description.
     */
    virtual void addConfigFile(
        boost::program_options::options_description &desc) = 0;
};

/**
 * @c TypedOption represents an option of concrete type. It implements builder
 * pattern.
 */
template <typename T> class TypedOption : public Option {
public:
    /**
     * Makes option required.
     * @return @c *this.
     */
    TypedOption<T> &required();

    /**
     * Makes option positional.
     * @param name Long name of the command line option.
     * @param count Defines how many consecutive positional options should be
     * associated with this option.
     * @return @c *this.
     */
    TypedOption<T> &positional(const std::string &name, int count);

    /**
     * Makes option into a switch, i.e. a flag that does not accept any
     * argument.
     * @return @c *this.
     */
    TypedOption<T> &asSwitch();

    /**
     * Sets short name for a command line option.
     * @param name A short command line option name.
     * @return @c *this.
     */
    TypedOption<T> &withShortName(const std::string &name);

    /**
     * Sets long name for a command line option.
     * @param name A long command line option name.
     * @return @c *this.
     */
    TypedOption<T> &withLongName(const std::string &name);

    /**
     * Sets name for a configuration file option.
     * @param name A configuration file option name.
     * @return @c *this.
     */
    TypedOption<T> &withConfigName(const std::string &name);

    /**
     * Sets name for an environment option.
     * @param name An environment option name.
     * @return @c *this.
     */
    TypedOption<T> &withEnvName(const std::string &name);

    /**
     * Sets name for a value, that will be shown in the help message.
     * @param name A value name.
     * @return @c *this.
     */
    TypedOption<T> &withValueName(const std::string &name);

    /**
     * Sets implicit option value.
     * @param value Implicit option value.
     * @return @c *this.
     */
    TypedOption<T> &withImplicitValue(const T &value);

    /**
     * Sets default option value.
     * @param value Default option value.
     * @param description Description that will be shown in the help message.
     * @return @c *this.
     */
    TypedOption<T> &withDefaultValue(
        const T &value, const std::string &description);

    /**
     * Sets option group.
     * @param group A @c OptionGroup.
     * @return @c *this.
     */
    TypedOption<T> &withGroup(OptionGroup group);

    /**
     * Sets option description.
     * @param description Description that will be shown in the help message.
     * @return @c *this.
     */
    TypedOption<T> &withDescription(const std::string &description);

    bool hasGroup(const OptionGroup &group) const override;

    bool isSet(const boost::program_options::variables_map &vm) const override;

    std::string name() const override;

    void addCommandLine(
        boost::program_options::options_description &desc) override;

    void addPositional(
        boost::program_options::positional_options_description &desc) override;

    void addEnvironment(
        boost::program_options::options_description &desc) override;

    void addConfigFile(
        boost::program_options::options_description &desc) override;

private:
    void add(boost::program_options::options_description &desc,
        const std::string &name);

    T m_value;
    bool m_required = false;
    bool m_positional = false;
    bool m_switch = false;
    int m_positionalCount = 0;
    boost::optional<std::string> m_shortName;
    boost::optional<std::string> m_longName;
    boost::optional<std::string> m_configName;
    boost::optional<std::string> m_envName;
    boost::optional<std::string> m_valueName;
    boost::optional<T> m_implicitValue;
    boost::optional<T> m_defaultValue;
    std::string m_defaultValueDescription;
    boost::optional<OptionGroup> m_group;
    boost::optional<std::string> m_description;
};

template <typename T> TypedOption<T> &TypedOption<T>::required()
{
    m_required = true;
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::positional(const std::string &name, int count)
{
    m_positional = true;
    m_longName.reset(name);
    m_positionalCount = count;
    return *this;
}

template <typename T> TypedOption<T> &TypedOption<T>::asSwitch()
{
    m_switch = true;
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withShortName(const std::string &name)
{
    m_shortName.reset(name);
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withLongName(const std::string &name)
{
    m_longName.reset(name);
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withConfigName(const std::string &name)
{
    m_configName.reset(name);
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withEnvName(const std::string &name)
{
    m_envName.reset(name);
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withValueName(const std::string &name)
{
    m_valueName.reset(name);
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withImplicitValue(const T &value)
{
    m_implicitValue.reset(value);
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withDefaultValue(
    const T &value, const std::string &description)
{
    m_defaultValue.reset(value);
    m_defaultValueDescription = description;
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withGroup(OptionGroup group)
{
    m_group.reset(group);
    return *this;
}

template <typename T>
TypedOption<T> &TypedOption<T>::withDescription(const std::string &description)
{
    m_description.reset(description);
    return *this;
}

template <typename T>
bool TypedOption<T>::hasGroup(const OptionGroup &group) const
{
    return m_group && m_group.get() == group;
}

template <typename T>
bool TypedOption<T>::isSet(
    const boost::program_options::variables_map &vm) const
{
    return (m_longName && vm.count(m_longName.get())) ||
        (m_shortName && vm.count("-" + m_shortName.get())) ||
        (m_envName && vm.count(m_envName.get())) ||
        (m_configName && vm.count(m_configName.get()));
}

template <typename T> std::string TypedOption<T>::name() const
{
    return m_longName.get_value_or(m_shortName.get_value_or(
        m_envName.get_value_or(m_configName.get_value_or(""))));
}

template <typename T>
void TypedOption<T>::addCommandLine(
    boost::program_options::options_description &desc)
{
    if (!m_shortName && !m_longName)
        return;

    std::stringstream name;
    name << m_longName.get_value_or("");
    name << "," << m_shortName.get_value_or("");

    add(desc, name.str());
}

template <typename T>
void TypedOption<T>::addPositional(
    boost::program_options::positional_options_description &desc)
{
    if (!m_positional)
        return;

    desc.add(m_longName.get().c_str(), m_positionalCount);
}

template <typename T>
void TypedOption<T>::addEnvironment(
    boost::program_options::options_description &desc)
{
    if (!m_envName)
        return;

    add(desc, m_envName.get());
}

template <typename T>
void TypedOption<T>::addConfigFile(
    boost::program_options::options_description &desc)
{
    if (!m_configName)
        return;

    add(desc, m_configName.get());
}

template <typename T>
void TypedOption<T>::add(
    boost::program_options::options_description &desc, const std::string &name)
{
    auto value = boost::program_options::value<T>(&m_value);

    if (m_switch) {
        value->zero_tokens();
    }

    value->value_name(m_valueName.get_value_or("arg"));

    if (m_implicitValue) {
        value->implicit_value(m_implicitValue.get(), "");
    }

    if (m_defaultValue) {
        value->default_value(m_defaultValue.get(), m_defaultValueDescription);
    }

    if (m_required) {
        value->required();
    }

    desc.add_options()(
        name.c_str(), std::move(value), m_description.get_value_or("").c_str());
}

} // namespace options
} // namespace client
} // namespace one

#endif // ONECLIENT_OPTION_H
