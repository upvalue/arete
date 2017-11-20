# run-tests.py - A simple test runner that compares output to expected output pre-written into files.

import glob
import subprocess
import os
import sys
import re

successful_tests = 0
total_tests = 0
print_errors = False
ansi_escape = re.compile(r'\x1b[^m]*m')

def arete_line(x):
    return not ansi_escape.sub('', x).startswith('arete:')

def run_tests(path, args = []):
    global successful_tests, total_tests
    sys.stdout.write('running %s tests: ' % path)

    tests = []

    # Get path of all tests
    test_paths = glob.glob("tests/%s/*.scm" % path) + glob.glob("tests/%s/*.sld" % path)

    for path in test_paths:
        expect_error = False

        result_path = path[:-3] + 'exp'
        if not os.path.exists(result_path):
            if os.path.exists(path[:-3] + 'err'):
                expect_error = True
            else:
                continue

        total_tests += 1

        mapped_args = [a for a in map(lambda arg: arg.format(path), args)]

        tests.append((path, result_path, expect_error, mapped_args))

    #print(tests)

    for test in tests:
        (test_path, result_path, expect_error, args) = test

        kmd = ['./bin/arete'] + args
        cmd = subprocess.Popen(kmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        cmd_out, cmd_err = cmd.communicate()

        if expect_error and cmd.returncode == 0:
            print('\n-- %s was supposed to error but didn\'t:\n%s\n' % (test_path, cmd_err.decode('utf-8').strip()))
            print('!')
        elif not expect_error and cmd.returncode != 0:
            print('\n-- %s errored with text:\n%s\n' % (test_path, cmd_err.decode('utf-8').strip()))
            print('!')
            continue

        if not expect_error:
            result = cmd_out.decode('utf-8').strip()
            expected = ''

            with open(result_path, 'r') as f:
                expected = f.read().strip()
                
            # Ignore all debug lines
            result = '\n'.join(filter(arete_line, [line for line in result.split('\n')]))

            if expected != result:
                print('\n-- %s failed, expected\n%s\n-- but got\n%s' % (test_path, expected, result))
                print('!')
                continue

        sys.stdout.write('+')

        successful_tests += 1

    print('')

suites = (
    ('reader', ['--read', '{}']),
    ('preboot',  ['{}']),
    ('expander', ['scheme/expand.scm', 'scheme/syntax.scm', '{}']),
    ('compiler',  ['--set', 'compiler-test-file', '"{}"',
                   'tests/compiler-test.scm'])
)

if __name__ == '__main__':
    import sys

    if len(sys.argv[1:]) == 0:
        for suite in suites:
            run_tests(*suite)
    else:
        for suite in suites:
            if suite[0] in sys.argv[1:]:
                run_tests(*suite)


    print('%s out of %s tests successful' % (successful_tests, total_tests))

    if successful_tests != total_tests:
        sys.exit(1)

