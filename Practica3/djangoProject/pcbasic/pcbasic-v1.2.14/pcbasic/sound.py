"""
PC-BASIC - sound.py
Sound handling

(c) 2013, 2014, 2015 Rob Hagemans
This file is released under the GNU GPL version 3.
"""

import Queue
import threading
import time

try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO

import error
import config
import state
import util
import draw_and_play
import representation
import vartypes
import backend


class PersistentQueue(Queue.Queue):
    """ Simple picklable Queue. """

    def __getstate__(self):
        """ Get pickling dict for queue. """
        qlist = []
        while True:
            try:
                qlist.append(self.get(False))
                self.task_done()
            except Queue.Empty:
                break
        return { 'qlist': qlist }

    def __setstate__(self, st):
        """ Initialise queue from pickling dict. """
        self.__init__()
        qlist = st['qlist']
        for item in qlist:
            self.put(item)


message_queue = Queue.Queue()
tone_queue = None

# audio plugin
audio = None

# sound capabilities - '', 'pcjr' or 'tandy'
pcjr_sound = ''

# base frequency for noise source
base_freq = 3579545./1024.

# 12-tone equal temperament
# C, C#, D, D#, E, F, F#, G, G#, A, A#, B
note_freq = [ 440.*2**((i-33.)/12.) for i in range(84) ]
notes = {   'C':0, 'C#':1, 'D-':1, 'D':2, 'D#':3, 'E-':3, 'E':4, 'F':5, 'F#':6,
            'G-':6, 'G':7, 'G#':8, 'A-':8, 'A':9, 'A#':10, 'B-':10, 'B':11 }



class AudioEvent(object):
    """ Signal object for audio queue. """

    def __init__(self, event_type, params=None):
        """ Create signal. """
        self.event_type = event_type
        self.params = params


# audio queue signals
AUDIO_TONE = 0
AUDIO_STOP = 1
AUDIO_NOISE = 2
AUDIO_QUIT = 4
AUDIO_PERSIST = 6



def prepare():
    """ Prepare the audio subsystem. """
    global pcjr_sound
    # pcjr/tandy sound
    if config.get('syntax') in ('pcjr', 'tandy'):
        pcjr_sound = config.get('syntax')
    # initialise sound queue
    state.console_state.sound = Sound()
    # tandy has SOUND ON by default, pcjr has it OFF
    state.console_state.sound.sound_on = (pcjr_sound == 'tandy')
    # pc-speaker on/off; (not implemented; not sure whether should be on)
    state.console_state.sound.beep_on = True
    # persist tone queue
    state.console_state.tone_queue = [PersistentQueue(), PersistentQueue(),
                                      PersistentQueue(), PersistentQueue() ]

def init():
    """ Initialise the audio backend. """
    global audio, tone_queue
    # NOTE that we shouldn't assign to either of these queues after this point
    tone_queue = state.console_state.tone_queue
    if not audio or not audio.init():
        return False
    return True


class PlayState(object):
    """ State variables of the PLAY command. """

    def __init__(self):
        """ Initialise play state. """
        self.octave = 4
        self.speed = 7./8.
        self.tempo = 2. # 2*0.25 =0 .5 seconds per quarter note
        self.length = 0.25
        self.volume = 15


class Sound(object):
    """ Sound queue manipulations. """

    def __init__(self):
        """ Initialise sound queue. """
        # Tandy/PCjr noise generator
        # frequency for noise sources
        self.noise_freq = [base_freq / v for v in [1., 2., 4., 1., 1., 2., 4., 1.]]
        self.noise_freq[3] = 0.
        self.noise_freq[7] = 0.
        # Tandy/PCjr SOUND ON and BEEP ON
        self.sound_on = False
        self.beep_on = True
        self.reset()

    def reset(self):
        """ Reset PLAY state (CLEAR). """
        # music foreground (MF) mode
        self.foreground = True
        # reset all PLAY state
        self.play_state = [ PlayState(), PlayState(), PlayState() ]

    def beep(self):
        """ Play the BEEP sound. """
        self.play_sound(800, 0.25)

    def play_sound(self, frequency, duration, fill=1, loop=False, voice=0, volume=15, wait=True):
        """ Play a sound on the tone generator. """
        if frequency < 0:
            frequency = 0
        if ((pcjr_sound == 'tandy' or
                (pcjr_sound == 'pcjr' and self.sound_on)) and
                frequency < 110. and frequency != 0):
            # pcjr, tandy play low frequencies as 110Hz
            frequency = 110.
        tone = AudioEvent(AUDIO_TONE, (frequency, duration, fill, loop, volume))
        state.console_state.tone_queue[voice].put(tone)
        if voice == 2 and frequency != 0:
            # reset linked noise frequencies
            # /2 because we're using a 0x4000 rotation rather than 0x8000
            self.noise_freq[3] = frequency/2.
            self.noise_freq[7] = frequency/2.
        if wait:
            # at most 16 notes in the sound queue (not 32 as the guide says!)
            self.wait_music(15)

    def wait_music(self, wait_length=0):
        """ Wait until a given number of notes are left on the queue. """
        while (self.queue_length(0) > wait_length or
                self.queue_length(1) > wait_length or
                self.queue_length(2) > wait_length):
            backend.wait()

    def wait_all_music(self):
        """ Wait until all music (not noise) has finished playing. """
        while (self.is_playing(0) or self.is_playing(1) or self.is_playing(2)):
            backend.wait()

    def stop_all_sound(self):
        """ Terminate all sounds immediately. """
        for q in state.console_state.tone_queue:
            while not q.empty():
                try:
                    q.get(False)
                except Queue.Empty:
                    continue
                q.task_done()
        message_queue.put(AudioEvent(AUDIO_STOP))

    def play_noise(self, source, volume, duration, loop=False):
        """ Play a sound on the noise generator. """
        frequency = self.noise_freq[source]
        noise = AudioEvent(AUDIO_NOISE, (source > 3, frequency, duration, 1, loop, volume))
        state.console_state.tone_queue[3].put(noise)
        # don't wait for noise

    def queue_length(self, voice=0):
        """ Return the number of notes in the queue. """
        # NOTE: this returns zero when there are still TWO notes to play
        # one in the pre-play buffer and another because we subtract 1 here
        # this agrees with empirical GW-BASIC ON PLAY() timings!
        return max(0, tone_queue[voice].qsize()-1)

    def is_playing(self, voice):
        """ A note is playing or queued at the given voice. """
        return self.queue_length(voice) or audio.next_tone[voice]

    def persist(self, flag):
        """ Set mixer persistence flag (runmode). """
        message_queue.put(AudioEvent(AUDIO_PERSIST, flag))

    ### PLAY statement

    def play(self, mml_list):
        """ Parse a list of Music Macro Language strings. """
        gmls_list = []
        for mml in mml_list:
            gmls = StringIO()
            # don't convert to uppercase as VARPTR$ elements are case sensitive
            gmls.write(str(mml))
            gmls.seek(0)
            gmls_list.append(gmls)
        next_oct = 0
        total_time = [0, 0, 0, 0]
        voices = range(3)
        while True:
            if not voices:
                break
            for voice in voices:
                vstate = self.play_state[voice]
                gmls = gmls_list[voice]
                c = util.skip_read(gmls, draw_and_play.ml_whitepace).upper()
                if c == '':
                    voices.remove(voice)
                    continue
                elif c == ';':
                    continue
                elif c == 'X':
                    # execute substring
                    sub = draw_and_play.ml_parse_string(gmls)
                    pos = gmls.tell()
                    rest = gmls.read()
                    gmls.truncate(pos)
                    gmls.write(str(sub))
                    gmls.write(rest)
                    gmls.seek(pos)
                elif c == 'N':
                    note = draw_and_play.ml_parse_number(gmls)
                    util.range_check(0, 84, note)
                    dur = vstate.length
                    c = util.skip(gmls, draw_and_play.ml_whitepace).upper()
                    if c == '.':
                        gmls.read(1)
                        dur *= 1.5
                    if note == 0:
                        self.play_sound(0, dur*vstate.tempo, vstate.speed,
                                        volume=0, voice=voice)
                    else:
                        self.play_sound(note_freq[note-1], dur*vstate.tempo,
                                         vstate.speed, volume=vstate.volume,
                                         voice=voice)
                    total_time[voice] += dur*vstate.tempo
                elif c == 'L':
                    recip = draw_and_play.ml_parse_number(gmls)
                    util.range_check(1, 64, recip)
                    vstate.length = 1. / recip
                elif c == 'T':
                    recip = draw_and_play.ml_parse_number(gmls)
                    util.range_check(32, 255, recip)
                    vstate.tempo = 240. / recip
                elif c == 'O':
                    octave = draw_and_play.ml_parse_number(gmls)
                    util.range_check(0, 6, octave)
                    vstate.octave = octave
                elif c == '>':
                    vstate.octave += 1
                    if vstate.octave > 6:
                        vstate.octave = 6
                elif c == '<':
                    vstate.octave -= 1
                    if vstate.octave < 0:
                        vstate.octave = 0
                elif c in ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'P'):
                    note = c
                    dur = vstate.length
                    length = None
                    while True:
                        c = util.skip(gmls, draw_and_play.ml_whitepace).upper()
                        if not c:
                            break
                        elif c == '.':
                            gmls.read(1)
                            dur *= 1.5
                        elif c in representation.ascii_digits:
                            numstr = ''
                            while c and c in representation.ascii_digits:
                                gmls.read(1)
                                numstr += c
                                c = util.skip(gmls, draw_and_play.ml_whitepace)
                            # NOT ml_parse_number, only literals allowed here!
                            length = int(numstr)
                            util.range_check(0, 64, length)
                            if length > 0:
                                dur = 1. / float(length)
                        elif c in ('#', '+'):
                            gmls.read(1)
                            note += '#'
                        elif c == '-':
                            gmls.read(1)
                            note += '-'
                        else:
                            break
                    if note == 'P':
                        # length must be specified
                        if length is None:
                            raise error.RunError(error.IFC)
                        # don't do anything for length 0
                        elif length > 0:
                            self.play_sound(0, dur * vstate.tempo, vstate.speed,
                                            volume=vstate.volume, voice=voice)
                            total_time[voice] += dur*vstate.tempo
                    else:
                        # use default length for length 0
                        try:
                            self.play_sound(
                                note_freq[(vstate.octave+next_oct)*12 + notes[note]],
                                dur * vstate.tempo, vstate.speed,
                                volume=vstate.volume, voice=voice)
                            total_time[voice] += dur*vstate.tempo
                        except KeyError:
                            raise error.RunError(error.IFC)
                    next_oct = 0
                elif c == 'M':
                    c = util.skip_read(gmls, draw_and_play.ml_whitepace).upper()
                    if c == 'N':
                        vstate.speed = 7./8.
                    elif c == 'L':
                        vstate.speed = 1.
                    elif c == 'S':
                        vstate.speed = 3./4.
                    elif c == 'F':
                        self.foreground = True
                    elif c == 'B':
                        self.foreground = False
                    else:
                        raise error.RunError(error.IFC)
                elif c == 'V' and (pcjr_sound == 'tandy' or
                                    (pcjr_sound == 'pcjr' and self.sound_on)):
                    vstate.volume = min(15,
                                    max(0, draw_and_play.ml_parse_number(gmls)))
                else:
                    raise error.RunError(error.IFC)
        max_time = max(total_time)
        for voice in range(3):
            if total_time[voice] < max_time:
                self.play_sound(0, max_time - total_time[voice], 1, 0, voice)
        if self.foreground:
            self.wait_all_music()



###############################################################################

prepare()
