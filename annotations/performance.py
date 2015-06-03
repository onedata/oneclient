import os
import re
import sys
import copy
import json
import time
import traceback
from subprocess import Popen, PIPE

PERFORMANCE_ENV_VARIABLE = 'performance'
PERFORMANCE_RESULT_FILE = os.path.join(os.environ.get('base_test_dir', '.'),
                                       'performance.json')


class Parameter(object):
    """Input/output parameter used by performance test module."""

    def __init__(self, name='', description='', value=0, unit=''):
        self.name = name
        self.description = description
        self.value = value
        self.unit = unit

    def __repr__(self):
        return "Parameter(name: '{0}', description: '{1}', value: {2}," \
               " unit: '{3}')".format(self.name, self.description, self.value,
                                      self.unit)

    def aggregate_value(self, value):
        """Adds given value to the parameter value."""
        self.value += value
        return self

    def append_value(self, rep, value):
        """Appends value of given repeat to the parameter value."""
        self.value.update({str(rep): value})
        return self

    def average(self, n):
        """Returns average parameter value from n repeats."""
        param = copy.copy(self)
        param.value = int(param.value / n)
        return param

    def format(self):
        """Returns parameter fields as a dictionary."""
        return {'name': self.name,
                'description': self.description,
                'value': self.value,
                'unit': self.unit}


# noinspection PyDefaultArgument
def performance(config={}, skip=False):
    """Decorator that wraps test case and enables execution of performance
    tests configurations."""

    def performance_decorator(test_case):
        if os.environ.get(PERFORMANCE_ENV_VARIABLE) == 'True' and skip:
            pass
        else:
            default_reps = config.get('repeats', 1)
            default_params = config.get('parameters', [])

            def test_case_decorator(*args, **kwargs):
                if os.environ.get(PERFORMANCE_ENV_VARIABLE) == 'True':
                    test_suite = sys.modules[test_case.__module__]
                    configs = config.get('configs', {})
                    exec_perf_configs(test_suite, test_case, args, kwargs,
                                      default_reps, default_params, configs)
                else:
                    exec_ct_config(test_case, default_params, args, kwargs)

            return test_case_decorator

    return performance_decorator


def exec_ct_config(test_case, params, args, kwargs):
    """Executes integration test case using non-performance configuration."""

    kwargs['parameters'] = parameters_to_dict(params)
    test_case(*args, **kwargs)


def exec_perf_configs(test_suite, test_case, case_args, case_kwargs,
                      default_reps, default_params, configs):
    """Executes integration test case using performance configurations."""

    results = map(lambda (config_name, config):
                  exec_perf_config(test_suite, test_case, case_args,
                                   case_kwargs, config_name, config,
                                   default_reps, default_params),
                  configs.items())
    assert all(results)


# noinspection PyShadowingNames
def exec_perf_config(test_suite, test_case, case_args, case_kwargs, config_name,
                     config, default_reps, default_params):
    """Executes integration test case using performance configuration."""

    # Fetch and prepare test case configuration.
    suite_name = test_suite.__name__
    case_name = test_case.func_name
    config_reps = config.get('repeats', default_reps)
    config_descr = config.get('description', '')
    # Merge specific configuration test case parameters with default test case
    # parameters, so that specific values overrider default ones.
    config_params = merge_parameters(config.get('parameters', []),
                                     default_params)

    # Inject configuration parameters into test case parameters.
    case_kwargs['parameters'] = parameters_to_dict(config_params)
    reps_summary, reps_details, failed_reps = exec_test_repeats(test_case,
                                                                case_args,
                                                                case_kwargs,
                                                                config_reps)
    successful_reps = config_reps - len(failed_reps)
    reps_average = map(lambda param: param.average(successful_reps),
                       reps_summary)

    # Create JSON description of performance configuration execution.
    performance = load_performance_results()
    suites = performance.get('suites', {})
    cases = suites.get(suite_name, {}).get('cases', {})
    configs = cases.get(case_name, {}).get('configs', {})

    configs.update({config_name: {
        'name': config_name,
        'completed': get_timestamp(),
        'parameters': format_parameters(config_params),
        'description': config_descr,
        'repeats_number': config_reps,
        'successful_repeats_number': successful_reps,
        'successful_repeats_summary': format_parameters(reps_summary),
        'successful_repeats_average': format_parameters(reps_average),
        'successful_repeats_details': format_parameters(reps_details),
        'failed_repeats_details': failed_reps
    }})
    cases.update({case_name: {
        'name': case_name,
        'description': test_case.__doc__ or '',
        'configs': configs
    }})
    suites.update({suite_name: {
        'name': suite_name,
        'copyright': get_copyright(test_suite),
        'authors': get_authors(test_suite),
        'description': test_suite.__doc__ or '',
        'cases': cases
    }})
    performance.update({'suites': suites})

    save_performance_results(performance)

    # Check whether performance configuration execution has been successfully
    # completed.
    if failed_reps:
        return False
    return True


# noinspection PyShadowingNames
def exec_test_repeats(test_case, case_args, case_kwargs, reps):
    """Executes test case multiple times."""

    reps_summary = []
    reps_details = []
    failed_reps = {}
    for rep in xrange(1, reps + 1):
        status, result = exec_test_repeat(test_case, case_args, case_kwargs)
        if status:
            if not reps_summary:
                # Initialize list of parameters for test case repeats.
                reps_summary = copy.deepcopy(result)
                for param in result:
                    param.value = {rep: param.value}
                    reps_details.append(param)
            else:
                # Merge list of test case parameters from current test case
                # repeat with list of parameters from previous test case repeats.
                reps_summary = map(lambda (param1, param2):
                                   param1.aggregate_value(param2.value),
                                   zip(reps_summary, result))
                reps_details = map(lambda (param1, param2):
                                   param1.append_value(rep, param2.value),
                                   zip(reps_details, result))
        else:
            reps_details = map(lambda param: param.append_value(rep, 0),
                               reps_details)
            failed_reps.update({rep: result})
    return reps_summary, reps_details, failed_reps


def exec_test_repeat(test_case, case_args, case_kwargs):
    """Executes test case once."""

    try:
        start_time = time.time()
        result = test_case(*case_args, **case_kwargs)
        end_time = time.time()
        result = [result] if not isinstance(result, list) else result
        test_time = int((end_time - start_time) * 1000000)
        params = filter(lambda param: isinstance(param, Parameter), result)
        params.insert(0, Parameter(
            name='test_time',
            description='Test execution time.',
            value=test_time,
            unit='ms'))
        return True, params
    except Exception as e:
        return False, '{0}\n{1}'.format(e.message, traceback.format_exc())


# noinspection PyBroadException
def load_performance_results():
    """Loads performance test results from a file."""

    try:
        with open(PERFORMANCE_RESULT_FILE, 'r') as f:
            content = f.read()
            return json.loads(content).get('performance', {})
    except:
        # Fetch git repository metadata.
        return {
            'repository': cmd('basename `git rev-parse --show-toplevel`'),
            'branch': cmd('git rev-parse --abbrev-ref HEAD'),
            'commit': cmd('git rev-parse HEAD')
        }


def save_performance_results(content):
    """Saves performance test results into a file."""

    with open(PERFORMANCE_RESULT_FILE, 'w') as f:
        f.write(json.dumps({'performance': content}, indent=2,
                           separators=(',', ': ')))


def parameters_to_dict(params):
    """Transforms list of parameters into a dictionary, where parameter name is
    a key and parameter itself is a value."""

    return dict(map(lambda param: (param.name, param), params))


def format_parameters(params):
    """Returns list of formatted parameters."""

    return map(lambda param: param.format(), params)


# noinspection PyShadowingNames
def merge_parameters(params, default_params):
    """Merges list of config parameters and list of default parameters, so that
    default parameter value is overwritten by specific one."""

    params_names = set(map(lambda param: param.name, params))
    for param in default_params:
        if param.name not in params_names:
            params.append(param)
    return params


def get_timestamp():
    """Returns number of milliseconds since the Epoch."""

    return int(time.time() * 1000)


# noinspection PyBroadException
def get_copyright(test_suite):
    """Returns copyright for test suite."""

    try:
        return test_suite.__copyright__
    except:
        return ''


# noinspection PyBroadException
def get_authors(test_suite):
    """Returns authors of test suite."""

    try:
        return re.split(r'\s*,\s*', test_suite.__author__)
    except:
        return []


def cmd(command):
    """Executes command in shell of underlying operating system and returns
    standard output from command execution."""

    return Popen(command, shell=True, stdout=PIPE).stdout.read().strip()
