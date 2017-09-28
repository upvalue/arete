# run-tests.py - A simple test runner that compares output to expected output pre-written into files.

import glob
import subprocess
import sys

successful_tests = 0
total_tests = 0

def run_tests(path, args = []):
    global successful_tests, total_tests
    sys.stdout.write('running %s tests: ' % path)
    for test_path in glob.glob('tests/%s/*.scm' % path):
        total_tests += 1
        result_path = test_path[:-3] + 'exp'

        kmd = ['./arete', '--quiet'] + args + [test_path]
        cmd = subprocess.Popen(kmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

        cmd_out, cmd_err = cmd.communicate()

        if cmd.returncode != 0:
            print(cmd_err)
            print('\n-- %s errored with text:\n%s\n' % (test_path, cmd_err.decode('utf-8').strip()))
            continue

        sys.stdout.write('+')

        result = cmd_out.decode('utf-8').strip()
        expected = ''

        with open(result_path, 'r') as f:
            expected = f.read().strip()

        if expected != result:
            print('\n-- %s failed, expected\n%s\n-- but got\n%s' % (test_path, expected, result))
            continue

        successful_tests += 1

    print('')

run_tests('preboot', [])
run_tests('expander', ['boot.scm'])

print('%s out of %s tests successful' % (successful_tests, total_tests))
