"""Bound Python HTTP chains without accumulating timed-out worker threads."""
import queue
import threading


class BoundedSourceRequest:
    def __init__(self, label):
        self.label = label
        self._gate = threading.Lock()

    def run(self, callback, timeout):
        if not self._gate.acquire(blocking=False):
            raise TimeoutError(f"A previous {self.label} request is still finishing; retry shortly")
        completed = queue.Queue(maxsize=1)

        def work():
            try:
                completed.put((True, callback()))
            except Exception as exc:
                completed.put((False, exc))
            finally:
                self._gate.release()

        threading.Thread(target=work, daemon=True).start()
        try:
            ok, value = completed.get(timeout=timeout)
        except queue.Empty:
            raise TimeoutError(f"{self.label} request exceeded its time limit") from None
        if not ok:
            raise value
        return value
