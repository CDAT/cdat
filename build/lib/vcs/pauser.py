"Pausing the calculation, particularly after a plot."
import signal
def pause_handler (signum, frame):
    pass

def pause (time=2, handler = pause_handler):
    if time:
        old_handler = signal.getsignal(signal.SIGALRM)
        signal.signal(signal.SIGALRM, handler)
        signal.alarm(time)
        signal.pause()
        signal.signal(signal.SIGALRM, old_handler)

