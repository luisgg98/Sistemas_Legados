"""
PC-BASIC - typeface.py
Font handling

(c) 2014, 2015 Rob Hagemans
This file is released under the GNU GPL version 3.
"""

import os
import logging
import plat

def font_filename(name, height, ext='hex'):
    """ Return name_height.hex if filename exists in current path, else font_dir/name_height.hex. """
    name = '%s_%02d.%s' % (name, height, ext)
    if not os.path.exists(name):
        # if not found in current dir, try in font directory
        name = os.path.join(plat.font_dir, name)
    return name

def load(families, height, codepage_dict, nowarn=False):
    """ Load the specified fonts for a given CP-to-UTF8 codepage. """
    names = [ font_filename(name, height) for name in families ]
    fontfiles = [ open(name, 'rb') for name in reversed(names) if os.path.exists(name) ]
    if len(fontfiles) == 0:
        if not nowarn:
            logging.warning('Could not read font file for height %d', height)
        return None
    fontdict = load_hex(fontfiles, height, codepage_dict)
    for f in fontfiles:
        f.close()
    return build_codepage_font(fontdict, codepage_dict)

def load_hex(fontfiles, height, codepage_dict):
    """ Load a set of overlaying unifont .hex files. """
    fontdict = {}
    # use set() for speed - lookup is O(1) rather than O(n) for list
    codepoints_needed = set(codepage_dict.values())
    for fontfile in fontfiles:
        for line in fontfile:
            # ignore empty lines and comment lines (first char is #)
            if (not line) or (line[0] == '#'):
                continue
            # split unicodepoint and hex string (max 32 chars)
            ucshex = line[0:4]
            fonthex = line[5:69]
            # extract codepoint and hex string;
            # discard anything following whitespace; ignore malformed lines
            try:
                codepoint = int(ucshex, 16)
                # skip chars we won't need
                if codepoint not in codepoints_needed:
                    continue
                # skip chars we already have
                if (codepoint in fontdict):
                    continue
                # string must be 32-byte or 16-byte; cut to required font size
                if len(fonthex) < 64:
                    fonthex = fonthex[:32]
                if len(fonthex) < 32:
                    raise ValueError
                fontdict[codepoint] = fonthex.decode('hex')
            except Exception as e:
                logging.warning('Could not parse line in font file: %s', repr(line))
    return fontdict

def build_codepage_font(fontdict, codepage_dict):
    """ Extract the glyphs for a given codepage from a unicode font. """
    font = {}
    warnings = 0
    for ucs in codepage_dict:
        u = codepage_dict[ucs]
        try:
            font[ucs] = fontdict[u]
        except KeyError:
            warnings += 1
            if warnings <= 3:
                logging.debug('Codepoint %x [%s] not represented in font',
                              u, unicode(u).encode('utf-8'))
            if warnings == 3:
                logging.debug('Further codepoint warnings suppressed.')
    # char 0 should always be empty
    font['\0'] = '\0'*16
    return font

def fixfont(height, font, codepage_dict, font16):
    """ Fill in missing codepoints in font using 16-line font or blanks. """
    if not font:
        font = {}
    if height == 16:
        for c in codepage_dict:
            if c not in font:
                font[c] = ('\0'*16 if len(c) == 1 else '\0'*32)
    else:
        for c in codepage_dict:
            if c not in font:
                if font16 and c in font16:
                    font[c] = glyph_16_to(height, font16[c])
                else:
                    font[c] = ('\0'*height if len(c) == 1 else '\0'*2*height)
    return font

def glyph_16_to(height, glyph16):
    """ Crudely convert 16-line character to n-line character by taking out top and bottom. """
    s16 = list(glyph16)
    start = (16 - height) // 2
    if len(s16) == 16:
        return ''.join([ s16[i] for i in range(start, 16-start) ])
    else:
        return ''.join([ s16[i] for i in range(start*2, 32-start*2) ])
