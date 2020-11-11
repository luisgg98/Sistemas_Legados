"""
PC-BASIC - run.py
Main interpreter loop

(c) 2013, 2014, 2015, 2016 Rob Hagemans
This file is released under the GNU GPL version 3.
"""

import os
import sys
import logging
import traceback

import error
import util
import tokenise
import program
import statements
import console
import state
import backend
import reset
import flow
import debug
from representation import ascii_whitespace

# true if a prompt is needed on next cycle
state.basic_state.prompt = True
# input mode is AUTO (used by AUTO)
state.basic_state.auto_mode = False
# interpreter is executing a command
state.basic_state.execute_mode = False
# interpreter is waiting for INPUT or LINE INPUT
state.basic_state.input_mode = False
# previous interpreter mode
state.basic_state.last_mode = False, False

def start(cmd='', run=False, quit=False):
    """ Start the interpreter. """
    if cmd:
        store_line(cmd)
    if run:
        # run command before program
        if cmd:
            run_once()
        # position the pointer at start of program and enter execute mode
        flow.jump(None)
        state.basic_state.execute_mode = True
        state.console_state.screen.cursor.reset_visibility()
    # read-eval-print loop until quit or exception
    while True:
        run_once()
        if quit and state.console_state.keyb.buf.is_empty():
            break

def run_once():
    """ Read-eval-print loop: run once. """
    try:
        while True:
            state.basic_state.last_mode = state.basic_state.execute_mode, state.basic_state.auto_mode
            if state.basic_state.execute_mode:
                try:
                    # may raise Break
                    backend.check_events()
                    handle_basic_events()
                    if not statements.parse_statement():
                        state.basic_state.execute_mode = False
                except error.Break as e:
                    # ctrl-break stops foreground and background sound
                    state.console_state.sound.stop_all_sound()
                    handle_break(e)
            elif state.basic_state.auto_mode:
                try:
                    # auto step, checks events
                    auto_step()
                except error.Break:
                    # ctrl+break, ctrl-c both stop background sound
                    state.console_state.sound.stop_all_sound()
                    state.basic_state.auto_mode = False
            else:
                show_prompt()
                try:
                    # input loop, checks events
                    line = console.wait_screenline(from_start=True)
                    state.basic_state.prompt = not store_line(line)
                except error.Break:
                    state.console_state.sound.stop_all_sound()
                    state.basic_state.prompt = False
                    continue
            # change loop modes
            if switch_mode():
                break
    except error.RunError as e:
        handle_error(e)
        state.basic_state.prompt = True
    except error.Exit:
        raise
    except Exception as e:
        if debug.debug_mode:
            raise
        bluescreen(e)

def switch_mode():
    """ Switch loop mode. """
    last_execute, last_auto = state.basic_state.last_mode
    if state.basic_state.execute_mode != last_execute:
        # move pointer to the start of direct line (for both on and off!)
        flow.set_pointer(False, 0)
        state.console_state.screen.cursor.reset_visibility()
    return ((not state.basic_state.auto_mode) and
            (not state.basic_state.execute_mode) and last_execute)

def store_line(line):
    """ Store a program line or schedule a command line for execution. """
    if not line:
        return True
    state.basic_state.direct_line = tokenise.tokenise_line(line)
    c = util.peek(state.basic_state.direct_line)
    if c == '\0':
        # check for lines starting with numbers (6553 6) and empty lines
        program.check_number_start(state.basic_state.direct_line)
        program.store_line(state.basic_state.direct_line)
        reset.clear()
    elif c != '':
        # it is a command, go and execute
        state.basic_state.execute_mode = True
    return not state.basic_state.execute_mode

def show_prompt():
    """ Show the Ok or EDIT prompt, unless suppressed. """
    if state.basic_state.execute_mode:
        return
    if state.basic_state.edit_prompt:
        linenum, tell = state.basic_state.edit_prompt
        # unset edit prompt first, in case program.edit throws
        state.basic_state.edit_prompt = False
        program.edit(linenum, tell)
    elif state.basic_state.prompt:
        console.start_line()
        console.write_line("Ok\xff")

def auto_step():
    """ Generate an AUTO line number and wait for input. """
    numstr = str(state.basic_state.auto_linenum)
    console.write(numstr)
    if state.basic_state.auto_linenum in state.basic_state.line_numbers:
        console.write('*')
        line = bytearray(console.wait_screenline(from_start=True))
        if line[:len(numstr)+1] == numstr+'*':
            line[len(numstr)] = ' '
    else:
        console.write(' ')
        line = bytearray(console.wait_screenline(from_start=True))
    # run or store it; don't clear lines or raise undefined line number
    state.basic_state.direct_line = tokenise.tokenise_line(line)
    c = util.peek(state.basic_state.direct_line)
    if c == '\0':
        # check for lines starting with numbers (6553 6) and empty lines
        empty, scanline = program.check_number_start(state.basic_state.direct_line)
        if not empty:
            program.store_line(state.basic_state.direct_line)
            reset.clear()
        state.basic_state.auto_linenum = scanline + state.basic_state.auto_increment
    elif c != '':
        # it is a command, go and execute
        state.basic_state.execute_mode = True


############################
# event and error handling

def handle_basic_events():
    """ Jump to user-defined event subs if events triggered. """
    if state.basic_state.events.suspend_all or not state.basic_state.run_mode:
        return
    for event in state.basic_state.events.all:
        if (event.enabled and event.triggered
                and not event.stopped and event.gosub is not None):
            # release trigger
            event.triggered = False
            # stop this event while handling it
            event.stopped = True
            # execute 'ON ... GOSUB' subroutine;
            # attach handler to allow un-stopping event on RETURN
            flow.jump_gosub(event.gosub, event)

def handle_error(s):
    """ Handle a BASIC error through trapping or error message. """
    error.set_err(s)
    # not handled by ON ERROR, stop execution
    console.write_error_message(error.get_message(s.err), program.get_line_number(s.pos))
    state.basic_state.error_handle_mode = False
    state.basic_state.execute_mode = False
    state.basic_state.input_mode = False
    # special case: syntax error
    if s.err == error.STX:
        # for some reason, err is reset to zero by GW-BASIC in this case.
        state.basic_state.errn = 0
        if s.pos != -1:
            # line edit gadget appears
            state.basic_state.edit_prompt = (program.get_line_number(s.pos),
                                             state.basic_state.bytecode.tell())

def handle_break(e):
    """ Handle a Break event. """
    # print ^C at current position
    if not state.basic_state.input_mode and not e.stop:
        console.write('^C')
    # if we're in a program, save pointer
    if state.basic_state.run_mode:
        console.write_error_message("Break", program.get_line_number(e.pos))
        state.basic_state.stop = state.basic_state.bytecode.tell()
    else:
        console.write_error_message("Break", -1)
    state.basic_state.execute_mode = False
    state.basic_state.input_mode = False


def bluescreen(e):
    """ Display a modal exception message. """
    state.console_state.screen.screen(0, 0, 0, 0, new_width=80)
    console.clear()
    console.init_mode()
    exc_type, exc_value, exc_traceback = sys.exc_info()
    # log the standard python error
    logging.error(''.join(traceback.format_exception(exc_type, exc_value, exc_traceback)))
    # format the error more readably on the screen
    state.console_state.screen.set_border(4)
    state.console_state.screen.set_attr(0x70)
    console.write_line('EXCEPTION')
    state.console_state.screen.set_attr(15)
    if state.basic_state.run_mode:
        state.basic_state.bytecode.seek(-1, 1)
        program.edit(program.get_line_number(state.basic_state.bytecode.tell()),
                                         state.basic_state.bytecode.tell())
        console.write_line('\n')
    else:
        state.basic_state.direct_line.seek(0)
        console.write_line(str(tokenise.detokenise_compound_statement(state.basic_state.direct_line)[0])+'\n')
    stack = traceback.extract_tb(exc_traceback)
    for s in stack[-4:]:
        stack_line = '{0}:{1}, {2}'.format(
            os.path.split(s[0])[-1], s[1], s[2])
        stack_line_2 = '    {0}'.format(s[3])
        state.console_state.screen.set_attr(15)
        console.write_line(stack_line)
        state.console_state.screen.set_attr(7)
        console.write_line(stack_line_2)
    exc_message = traceback.format_exception_only(exc_type, exc_value)[0]
    state.console_state.screen.set_attr(15)
    console.write('{0}:'.format(exc_type.__name__))
    state.console_state.screen.set_attr(7)
    console.write_line(' {0}'.format(str(exc_value)))
    state.console_state.screen.set_attr(0x70)
    console.write_line(
        '\nThis is a bug in PC-BASIC.')
    state.console_state.screen.set_attr(7)
    console.write(
        'Sorry about that. Please file a bug report at\n   ')
    state.console_state.screen.set_attr(15)
    console.write(
        'https://sourceforge.net/p/pcbasic/discussion/bugs/')
    state.console_state.screen.set_attr(7)
    console.write(
        '\nor ')
    state.console_state.screen.set_attr(15)
    console.write(
        'https://github.com/robhagemans/pcbasic/issues')
    state.console_state.screen.set_attr(7)
    console.write_line(
        '\nPlease include the messages above and')
    console.write_line('as much information as you can about what you were doing and how this happened.')
    console.write_line('Thank you!')
    state.console_state.screen.set_attr(7)
    flow.set_pointer(False)
