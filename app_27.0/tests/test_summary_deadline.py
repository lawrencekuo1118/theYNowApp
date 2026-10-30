"""Exercise production deadline logic without importing network packages."""
import ast
import pathlib
import queue
import threading
import time

import sys
sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))
from source_deadline import BoundedSourceRequest
request = BoundedSourceRequest("test")
run = request.run
assert run(lambda: {'company_name': 'test'}, .1)['company_name'] == 'test'
try:
    run(lambda: (_ for _ in ()).throw(ValueError('source failure')), .1)
except ValueError:
    pass
else:
    raise AssertionError('source failure was hidden')
started = threading.Event()
finish = threading.Event()
def blocked():
    started.set()
    finish.wait(1)
try:
    run(blocked, .02)
except TimeoutError:
    pass
else:
    raise AssertionError('deadline did not return')
assert started.is_set()
try:
    run(lambda: 42, .1)
except TimeoutError:
    pass
else:
    raise AssertionError('retry accumulated another blocked request')
finish.set()
for _ in range(100):
    if not request._gate.locked():
        break
    time.sleep(.001)
assert run(lambda: 42, .1) == 42
print('PASS: Summary success/error/deadline, bounded in-flight retries, recovery')
