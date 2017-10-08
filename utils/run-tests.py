# run-tests.py - A simple test runner that compares output to expected output pre-written into files.

import glob
import subprocess
import os
import sys

successful_tests = 0
total_tests = 0
print_errors = False

def run_tests(path, args = []):
    global successful_tests, total_tests
    sys.stdout.write('running %s tests: ' % path)
    for test_path in glob.glob('tests/%s/*.scm' % path) + glob.glob('tests/%s/*.sld' % path):
        result_path = test_path[:-3] + 'exp'
        expect_error = False

        if not os.path.exists(result_path):
            if os.path.exists(test_path[:-3] + 'err'):
                expect_error = True
            else:
                continue
        total_tests += 1

        kmd = ['./arete'] + args + [test_path]
        cmd = subprocess.Popen(kmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        cmd_out, cmd_err = cmd.communicate()

        if expect_error and cmd.returncode == 0:
            print('\n-- %s was supposed to error but didn\'t:\n%s\n' % (test_path, cmd_err.decode('utf-8').strip()))
            print('!')
        elif not expect_error and cmd.returncode != 0:
            print('\n-- %s errored with text:\n%s\n' % (test_path, cmd_err.decode('utf-8').strip()))
            print('!')
            continue

        # print(cmd_err.decode('utf-8').strip())

        if not expect_error:
            result = cmd_out.decode('utf-8').strip()
            expected = ''

            with open(result_path, 'r') as f:
                expected = f.read().strip()

            if expected != result:
                print('\n-- %s failed, expected\n%s\n-- but got\n%s' % (test_path, expected, result))
                print('!')
                continue

        sys.stdout.write('+')

        successful_tests += 1

    print('')

suites = (
    ('reader', ['--read']),
#    ('preboot', []),
#    ('expander', ['boot.scm']),
#    ('modules', ['boot.scm', '--push-load-path=tests/modules'])
)

if __name__ == '__main__':

    import sys

    for arg in sys.argv[1:]:
        if arg not in suites:
            print('no such test suite %s' % arg)
            continue
        run_tests(arg, suites[arg])

    if len(sys.argv[1:]) == 0:
        for (path, args) in suites:
            run_tests(path, args)

    print('%s out of %s tests successful' % (successful_tests, total_tests))

    if successful_tests != total_tests:
        sys.exit(1)

