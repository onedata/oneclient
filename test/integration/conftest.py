import collections
import json
import os
import re
import subprocess
import sys
import traceback

import pytest

script_dir = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, script_dir)
from test_common import *
from environment import appmock, common, docker
from appmock_client import AppmockClient

PERFORMANCE_RESULT_FILE = \
    os.path.join(os.environ.get('BASE_TEST_DIR', '.'), 'performance.json')


@pytest.fixture(scope='module')
def _appmock_client(request):
    test_dir = os.path.dirname(os.path.realpath(request.module.__file__))

    result = appmock.up(image='onedata/builder', bindir=appmock_dir,
                        dns_server='none', uid=common.generate_uid(),
                        config_path=os.path.join(test_dir, 'env.json'))

    [container] = result['docker_ids']
    appmock_ip = docker.inspect(container)['NetworkSettings']['IPAddress']. \
        encode('ascii')

    def fin():
        docker.remove([container], force=True, volumes=True)

    request.addfinalizer(fin)
    return AppmockClient(appmock_ip)


@pytest.fixture
def appmock_client(request, _appmock_client):
    _appmock_client.reset_rest_history()
    _appmock_client.reset_tcp_history()
    return _appmock_client


@pytest.fixture
def result():
    return PerformanceResult()


def pytest_addoption(parser):
    parser.addoption('--performance', '-P', action='store_true',
                     help='run performance tests')


def pytest_configure(config):
    config.addinivalue_line(
        'markers',
        'performance(config): mark test to run in performance tests with config'
    )

    config.performance_report = {}


def pytest_generate_tests(metafunc):
    if hasattr(metafunc.function, 'performance'):
        kwargs = metafunc.function.performance.kwargs
        repeats = kwargs.get('repeats', 1)
        params = kwargs.get('parameters', [])
        configs = kwargs.get('configs', {})

        params = collections.OrderedDict(
            [(p.name, p.normalized_value()) for p in params])

        if not metafunc.config.getoption('--performance'):
            if params:
                metafunc.parametrize(params.keys(), [params.values()])
        else:
            metafunc.fixturenames.extend(['config_name', 'rep'])

            params_names = ['config_name', 'rep'] + params.keys()
            params_values = []

            for config_name, config in configs.items():
                current_params = params.copy()

                for p in config.get('parameters', []):
                    current_params[p.name] = p.normalized_value()

                for rep in range(1, repeats + 1):
                    params_values.append(
                        [config_name, rep] + current_params.values())

            metafunc.parametrize(params_names, params_values)


def pytest_collection_modifyitems(config, items):
    if config.getoption('--performance'):
        items[:] = [i for i in items if 'performance' in i.keywords]


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    outcome = yield
    report = outcome.get_result()
    perfmarker = item.get_marker('performance')

    if call.when == 'call' and item.config.getoption('-P') and perfmarker:
        suite = sys.modules[item.function.__module__]
        suite_name = suite.__name__
        case_name = item.function.__name__
        repeat = item.funcargs['rep']
        config_name = item.funcargs['config_name']
        test_config = perfmarker.kwargs['configs'][config_name]

        params = {p.name: p for p in perfmarker.kwargs['parameters']}
        params.update({p.name: p for p in test_config['parameters']})

        performance = item.config.performance_report
        suites = performance.get('suites', {})
        cases = suites.get(suite_name, {}).get('cases', {})
        configs = cases.get(case_name, {}).get('configs', {})
        results = configs.get(config_name, {}).get('results', {})
        failures = configs.get(config_name, {}).get('failures', {})

        if call.excinfo:
            failures[repeat] = call.excinfo
        else:
            test_duration_param = Parameter(
                name='test_time',
                description='Test execution time.',
                value=report.duration * 1000,
                unit='ms')

            results[repeat] = [test_duration_param] + item.funcargs.get(
                'result', PerformanceResult()).value

        configs.update({config_name: {
            'name': config_name,
            'completed': int(time.time() * 1000),
            'parameters': [p.format() for p in params.values()],
            'description': test_config.get('description', ''),
            'repeats_number': perfmarker.kwargs.get('repeats', 1),
            'results': results,
            'failures': failures,
        }})

        cases.update({case_name: {
            'name': case_name,
            'description': item.function.__doc__ or '',
            'configs': configs
        }})

        suites.update({suite_name: {
            'name': suite_name,
            'copyright': get_copyright(suite),
            'authors': get_authors(suite),
            'description': suite.__doc__ or '',
            'cases': cases
        }})

        performance['suites'] = suites


def pytest_unconfigure(config):
    for suite in config.performance_report.get('suites', {}).values():
        for case in suite.get('cases', {}).values():
            for cfg in case.get('configs', {}).values():
                results = cfg.pop('results', [])
                failures = cfg.pop('failures', [])

                reps_summary = []
                reps_details = []
                if results:
                    rep, params = results.items()[0]
                    reps_summary = copy.deepcopy(params)
                    for param in params:
                        param.value = {rep: param.value}
                        reps_details.append(param)

                for rep, params in results.items()[1:]:
                    for i, param in enumerate(params):
                        reps_summary[i].aggregate_value(param.value)
                        reps_details[i].append_value(rep, param.value)

                reps_average = [p.average(len(results)) for p in reps_summary]

                reps_summary = [p.format() for p in reps_summary]
                reps_details = [p.format() for p in reps_details]
                reps_average = [p.format() for p in reps_average]
                fail_details = {
                    rep: ''.join(
                        traceback.format_exception(e.type, e.value, e.tb))
                    for rep, e in failures.items()}

                cfg.update({
                    'successful_repeats_number': len(results),
                    'successful_repeats_summary': reps_summary,
                    'successful_repeats_average': reps_average,
                    'successful_repeats_details': reps_details,
                    'failed_repeats_details': fail_details
                })

    toplevel = subprocess.check_output(['git', 'rev-parse', '--show-toplevel'])
    commit = subprocess.check_output(['git', 'rev-parse', 'HEAD'])
    branch = subprocess.check_output(
        ['git', 'rev-parse', '--abbrev-ref', 'HEAD'])

    performance = {'performance': config.performance_report}
    performance['performance'].update({
        'repository': os.path.basename(toplevel).strip(),
        'branch': branch.strip(),
        'commit': commit.strip()
    })

    with open(PERFORMANCE_RESULT_FILE, 'w') as f:
        f.write(json.dumps(performance, indent=2, separators=(',', ': ')))


def get_copyright(mod):
    return mod.__copyright__ if hasattr(mod, '__copyright__') else ''


def get_authors(mod):
    author = mod.__author__ if hasattr(mod, '__author__') else ''
    return re.split(r'\s*,\s*', author)
