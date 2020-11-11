"""
PC-BASIC - audio_none.py
Null sound implementation

(c) 2013, 2014, 2015 Rob Hagemans
This file is released under the GNU GPL version 3.
"""

import datetime
import time
import threading
import Queue

import sound

##############################################################################
# interface

def init():
    """ Initialise sound system. """

    launch_thread()
    return True

def close():
    """ Clean up and exit sound system. """
    # drain signal queue (to allow for persistence) and request exit
    if sound.message_queue:
        sound.message_queue.put(sound.AudioEvent(sound.AUDIO_QUIT))
        sound.message_queue.join()
    # don't wait for tone que, it will not drain but be pickled later.
    if thread and thread.is_alive():
        # signal quit and wait for thread to finish
        thread.join()


# sound generators for sounds being queued
# if not None, something is playing
next_tone = [ None, None, None, None ]

##############################################################################
# implementation

thread = None

tick_s = 0.024

def launch_thread():
    """ Launch consumer thread. """
    global thread
    thread = threading.Thread(target=consumer_thread)
    thread.start()

def consumer_thread():
    """ Audio signal queue consumer thread. """
    while drain_message_queue():
        empty = drain_tone_queue()
        play_sound()
        # do not hog cpu
        if empty and next_tone == [None, None, None, None]:
            time.sleep(tick_s)

def drain_message_queue():
    """ Drain signal queue. """
    global next_tone
    alive = True
    while alive:
        try:
            signal = sound.message_queue.get(False)
        except Queue.Empty:
            return True
        if signal.event_type == sound.AUDIO_STOP:
            # stop all channels
            next_tone = [None, None, None, None]
        elif signal.event_type == sound.AUDIO_QUIT:
            # close thread after task_done
            alive = False
        # drop other messages
        sound.message_queue.task_done()

def drain_tone_queue():
    """ Drain signal queue. """
    global next_tone
    empty = False
    while not empty:
        empty = True
        for voice, q in enumerate(sound.tone_queue):
            if next_tone[voice] is None:
                try:
                    signal = q.get(False)
                    empty = False
                except Queue.Empty:
                    continue
                duration = 0
                if signal.event_type == sound.AUDIO_TONE:
                    # enqueue a tone
                    frequency, duration, fill, loop, volume = signal.params
                elif signal.event_type == sound.AUDIO_NOISE:
                    # enqueue a noise
                    is_white, frequency, duration, fill, loop, volume = signal.params
                latest = next_tone[voice] or datetime.datetime.now()
                next_tone[voice] = latest + datetime.timedelta(seconds=duration)
    return empty

def play_sound():
    """ Play sounds. """
    # handle playing queues
    now = datetime.datetime.now()
    for voice in range(4):
        if next_tone[voice] is not None and now >= next_tone[voice]:
            next_tone[voice] = None
            sound.tone_queue[voice].task_done()
