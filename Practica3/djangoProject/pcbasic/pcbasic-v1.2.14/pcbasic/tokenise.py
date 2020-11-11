"""
PC-BASIC - tokenise.py
Convert between tokenised and ASCII formats of a GW-BASIC program file

(c) 2013, 2014, 2015 Rob Hagemans
This file is released under the GNU GPL version 3.
"""

from string import ascii_uppercase
from string import digits as ascii_digits
import string

try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO

import config
import basictoken as tk
import representation
import util
import vartypes

# newline is considered whitespace: ' ', '\t', '\n'
from representation import ascii_whitespace
from util import name_chars

ascii_operators = '+-=/\\^*<>'

# keywords than can followed by one or more line numbers
linenum_words = [tk.keyword[token] for token in
    (tk.GOTO, tk.THEN, tk.ELSE, tk.GOSUB,
    tk.LIST, tk.RENUM, tk.EDIT, tk.LLIST,
    tk.DELETE, tk.RUN, tk.RESUME, tk.AUTO,
    tk.ERL, tk.RESTORE, tk.RETURN)]

#################################################################

def prepare():
    """ Initialise tokenise module. """
    global keyword_to_token, token_to_keyword
    token_to_keyword = tk.keyword
    if config.get('debug'):
        # NOTE: PC-BASIC only. Not the same command or token as Sperry DEBUG.
        token_to_keyword[tk.DEBUG] = 'DEBUG'
    if config.get('syntax') in ('pcjr', 'tandy'):
        # pcjr, tandy; incompatible with Sperry PC.
        token_to_keyword[tk.NOISE] = 'NOISE'
        token_to_keyword[tk.TERM] = 'TERM'
    keyword_to_token = dict(
            (reversed(item) for item in token_to_keyword.items()))

#################################################################
# Detokenise functions

def ascii_read_to(ins, findrange):
    """ Read until a character from a given range is found. """
    out = ''
    while True:
        d = ins.read(1)
        if d == '':
            break
        if d in findrange:
            break
        out += d
    ins.seek(-len(d),1)
    return out

def detokenise_line(ins, bytepos=None):
    """ Convert a tokenised program line to ascii text. """
    current_line = util.parse_line_number(ins)
    if current_line < 0:
        # parse_line_number has returned -1 and left us at: .. 00 | _00_ 00 1A
        # stream ends or end of file sequence \x00\x00\x1A
        return -1, '', 0
    elif current_line == 0 and util.peek(ins) == ' ':
        # ignore up to one space after line number 0
        ins.read(1)
    linum = bytearray(str(current_line))
    # write one extra whitespace character after line number
    # unless first char is TAB
    if util.peek(ins) != '\t':
        linum += bytearray(' ')
    line, textpos = detokenise_compound_statement(ins, bytepos)
    return current_line, linum + line, textpos + len(linum) + 1

def detokenise_compound_statement(ins, bytepos=None):
    """ Detokenise tokens until end of line. """
    litstring, comment = False, False
    textpos = 0
    output = bytearray()
    while True:
        s = ins.read(1)
        if not textpos and ins.tell() >= bytepos:
            textpos = len(output)
        if s in tk.end_line:
            # \x00 ends lines and comments when listed,
            # if not inside a number constant
            # stream ended or end of line
            break
        elif s == '"':
            # start of literal string, passed verbatim
            # until a closing quote or EOL comes by
            # however number codes are *printed* as the corresponding numbers,
            # even inside comments & literals
            output += s
            litstring = not litstring
        elif s in tk.number:
            ins.seek(-1, 1)
            representation.detokenise_number(ins, output)
        elif s in tk.linenum:
            # 0D: line pointer (unsigned int) - this token should not be here;
            #     interpret as line number and carry on
            # 0E: line number (unsigned int)
            output += representation.uint_to_str(bytearray(ins.read(2)))
        elif comment or litstring or ('\x20' <= s <= '\x7E'):
            # honest ASCII
            output += s
        elif s == '\x0A':
            # LF becomes LF CR
            output += '\x0A\x0D'
        elif s <= '\x09':
            # controls that do not double as tokens
            output += s
        else:
            ins.seek(-1, 1)
            comment = detokenise_keyword(ins, output)
    return output, textpos

def detokenise_keyword(ins, output):
    """ Convert a one- or two-byte keyword token to ascii. """
    # try for single-byte token or two-byte token
    # if no match, first char is passed unchanged
    s = ins.read(1)
    try:
        keyword = token_to_keyword[s]
    except KeyError:
        s += util.peek(ins)
        try:
            keyword = token_to_keyword[s]
            ins.read(1)
        except KeyError:
            output += s[0]
            return False
    # when we're here, s is an actual keyword token.
    # letter or number followed by token is separated by a space
    if (output and chr(output[-1]) in (string.digits + string.ascii_letters) and s not in tk.operator):
        output += ' '
    output += keyword
    comment = False
    if keyword == "'":
        comment = True
    elif keyword == "REM":
        nxt = ins.read(1)
        if nxt == '':
            pass
        elif nxt == tk.O_REM: # '
            # if next char is token('), we have the special value REM'
            # -- replaced by ' below.
            output += "'"
        else:
            # otherwise, it's part of the comment or an EOL or whatever,
            # pass back to stream so it can be processed
            ins.seek(-1, 1)
        comment = True
    # check for special cases
    #   [:REM']   ->  [']
    if len(output) > 4 and str(output[-5:]) ==  ":REM'":
        output[:] = output[:-5] + "'"
    #   [WHILE+]  ->  [WHILE]
    elif len(output) > 5 and str(output[-6:]) == "WHILE+":
        output[:] = output[:-1]
    #   [:ELSE]  ->  [ELSE]
    # note that anything before ELSE gets cut off,
    # e.g. if we have 1ELSE instead of :ELSE it also becomes ELSE
    # SIC: len(output) > 4 and str(output[-4:])
    elif len(output) > 4 and str(output[-4:]) == "ELSE":
        if (len(output) > 5 and chr(output[-5]) == ':' and
                    chr(output[-6]) in ascii_digits):
            output[:] = output[:-5] + " ELSE"
        else:
            output[:] = output[:-5] + "ELSE"
    # token followed by token or number is separated by a space,
    # except operator tokens and SPC(, TAB(, FN, USR
    nxt = util.peek(ins)
    if (not comment and
            nxt.upper() not in tk.end_line + tk.operator +
                            (tk.O_REM, '"', ',', ';',  ' ', ':', '(', ')', '$',
                             '%', '!', '#', '_', '@', '~', '|', '`') and
            s not in tk.operator + tk.with_bracket + (tk.USR, tk.FN)):
        # excluding TAB( SPC( and FN. \xD9 is ', \xD1 is FN, \xD0 is USR.
        output += ' '
    return comment

#################################################################
# Tokenise functions

def tokenise_line(line):
    """ Convert an ascii program line to tokenised form. """
    ins = StringIO(line)
    outs = StringIO()
    # skip whitespace at start of line
    d = util.skip(ins, ascii_whitespace)
    if d == '':
        # empty line at EOF
        return outs
    # read the line number
    tokenise_line_number(ins, outs)
    # expect line number
    allow_jumpnum = False
    # expect number (6553 6 -> the 6 is encoded as \x17)
    allow_number = True
    # flag for SPC( or TAB( as numbers can follow the closing bracket
    spc_or_tab = False
    # parse through elements of line
    while True:
        # peek next character, convert to uppercase
        c = util.peek(ins).upper()
        # anything after NUL is ignored till EOL
        if c == '\0':
            ins.read(1)
            ascii_read_to(ins, ('', '\r'))
            break
        # end of line
        elif c in ('', '\r'):
            break
        # handle whitespace
        elif c in ascii_whitespace:
            ins.read(1)
            outs.write(c)
        # handle string literals
        elif util.peek(ins) == '"':
            tokenise_literal(ins, outs)
        # handle jump numbers
        elif allow_number and allow_jumpnum and c in ascii_digits + '.':
            tokenise_jump_number(ins, outs)
        # handle numbers
        # numbers following var names with no operator or token in between
        # should not be parsed, eg OPTION BASE 1
        # note we don't include leading signs, encoded as unary operators
        # number starting with . or & are always parsed
        elif c in ('&', '.') or (allow_number and
                                  not allow_jumpnum and c in ascii_digits):
            representation.tokenise_number(ins, outs)
        # operator keywords ('+', '-', '=', '/', '\\', '^', '*', '<', '>'):
        elif c in ascii_operators:
            ins.read(1)
            # operators don't affect line number mode - can do line number
            # arithmetic and RENUM will do the strangest things
            # this allows for 'LIST 100-200' etc.
            outs.write(keyword_to_token[c])
            allow_number = True
        # special case ' -> :REM'
        elif c == "'":
            ins.read(1)
            outs.write(':' + tk.REM + tk.O_REM)
            tokenise_rem(ins, outs)
        # special case ? -> PRINT
        elif c == '?':
            ins.read(1)
            outs.write(tk.PRINT)
            allow_number = True
        # keywords & variable names
        elif c in ascii_uppercase:
            word = tokenise_word(ins, outs)
            # handle non-parsing modes
            if (word in ('REM', "'") or
                            (word == 'DEBUG' and word in keyword_to_token)):
                tokenise_rem(ins, outs)
            elif word == "DATA":
                tokenise_data(ins, outs)
            else:
                allow_jumpnum = (word in linenum_words)
                # numbers can follow tokenised keywords
                # (which does not include the word 'AS')
                allow_number = (word in keyword_to_token)
                if word in ('SPC(', 'TAB('):
                    spc_or_tab = True
        else:
            ins.read(1)
            if c in (',', '#', ';'):
                # can separate numbers as well as jumpnums
                allow_number = True
            elif c in ('(', '['):
                allow_jumpnum, allow_number = False, True
            elif c == ')' and spc_or_tab:
                spc_or_tab = False
                allow_jumpnum, allow_number = False, True
            else:
                allow_jumpnum, allow_number = False, False
            # replace all other nonprinting chars by spaces;
            # HOUSE 0x7f is allowed.
            outs.write(c if ord(c) >= 32 and ord(c) <= 127 else ' ')
    outs.seek(0)
    return outs

def tokenise_rem(ins, outs):
    """ Pass anything after REM as is till EOL. """
    outs.write(ascii_read_to(ins, ('', '\r', '\0')))

def tokenise_data(ins, outs):
    """ Pass DATA as is, till end of statement, except for literals. """
    while True:
        outs.write(ascii_read_to(ins, ('', '\r', '\0', ':', '"')))
        if util.peek(ins) == '"':
            # string literal in DATA
            tokenise_literal(ins, outs)
        else:
            break

def tokenise_literal(ins, outs):
    """ Pass a string literal. """
    outs.write(ins.read(1))
    outs.write(ascii_read_to(ins, ('', '\r', '\0', '"') ))
    if util.peek(ins)=='"':
        outs.write(ins.read(1))

def tokenise_line_number(ins, outs):
    """ Convert an ascii line number to tokenised start-of-line. """
    linenum = tokenise_uint(ins)
    if linenum != '':
        # terminates last line and fills up the first char in the buffer
        # (that would be the magic number when written to file)
        # in direct mode, we'll know to expect a line number if the output
        # starts with a  00
        outs.write('\0')
        # write line number. first two bytes are for internal use
        # & can be anything nonzero; we use this.
        outs.write('\xC0\xDE' + linenum)
        # ignore single whitespace after line number, if any,
        # unless line number is zero (as does GW)
        if util.peek(ins) == ' ' and linenum != '\0\0' :
            ins.read(1)
    else:
        # direct line; internally, we need an anchor for the program pointer,
        # so we encode a ':'
        outs.write(':')

def tokenise_jump_number(ins, outs):
    """ Convert an ascii line number pointer to tokenised form. """
    word = tokenise_uint(ins)
    if word != '':
        outs.write(tk.T_UINT + word)
    elif util.peek(ins) == '.':
        ins.read(1)
        outs.write('.')

def tokenise_uint(ins):
    """ Convert an unsigned int (line number) to tokenised form. """
    word = bytearray()
    while True:
        c = ins.read(1)
        if c and c in ascii_digits + ascii_whitespace:
            word += c
        else:
            ins.seek(-len(c), 1)
            break
    # don't claim trailing w/s
    while len(word)>0 and chr(word[-1]) in ascii_whitespace:
        del word[-1]
        ins.seek(-1, 1)
    # remove all whitespace
    trimword = bytearray()
    for c in word:
        if chr(c) not in ascii_whitespace:
            trimword += chr(c)
    word = trimword
    # line number (jump)
    if len(word) > 0:
        if int(word) >= 65530:
            # note: anything >= 65530 is illegal in GW-BASIC
            # in loading an ASCII file, GWBASIC would interpret these as
            # '6553 1' etcetera, generating a syntax error on load.
            # keep 6553 as line number and push back the last number:
            ins.seek(4-len(word), 1)
            word = word[:4]
        return str(vartypes.integer_to_bytes(vartypes.int_to_integer_unsigned(int(word))))
    else:
        return ''

def tokenise_word(ins, outs):
    """ Convert a keyword to tokenised form. """
    word = ''
    while True:
        c = ins.read(1).upper()
        word += c
        # special cases 'GO     TO' -> 'GOTO', 'GO SUB' -> 'GOSUB'
        if word == 'GO':
            pos = ins.tell()
            # GO SUB allows 1 space
            if util.peek(ins, 4).upper() == ' SUB':
                word = 'GOSUB'
                ins.read(4)
            else:
                # GOTO allows any number of spaces
                nxt = util.skip(ins, ascii_whitespace)
                if ins.read(2).upper() == 'TO':
                    word = 'GOTO'
                else:
                    ins.seek(pos)
            if word in ('GOTO', 'GOSUB'):
                nxt = util.peek(ins).upper()
                if nxt and nxt in name_chars:
                    ins.seek(pos)
                    word = 'GO'
        if word in keyword_to_token:
            # ignore if part of a longer name, except FN, SPC(, TAB(, USR
            if word not in ('FN', 'SPC(', 'TAB(', 'USR'):
                nxt = util.peek(ins).upper()
                if nxt and nxt in name_chars:
                    continue
            token = keyword_to_token[word]
            # handle special case ELSE -> :ELSE
            if word == 'ELSE':
                outs.write(':' + token)
            # handle special case WHILE -> WHILE+
            elif word == 'WHILE':
                outs.write(token + tk.O_PLUS)
            else:
                outs.write(token)
            break
        # allowed names: letter + (letters, numbers, .)
        elif not c:
            outs.write(word)
            break
        elif c not in name_chars:
            word = word[:-1]
            ins.seek(-1, 1)
            outs.write(word)
            break
    return word

prepare()
