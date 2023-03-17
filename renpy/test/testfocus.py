# Copyright 2004-2023 Tom Rothamel <pytom@bishoujo.us>
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

from __future__ import division, absolute_import, with_statement, print_function, unicode_literals
from renpy.compat import PY2, basestring, bchr, bord, chr, open, pystr, range, round, str, tobytes, unicode # *


import renpy
import random


def find_focus(pattern):
    """
    Trues to find the focus with the shortest alt text containing `pattern`.
    If found, returns a random coordinate within that displayable.

    If `pattern` is None, returns a random coordinate that will trigger the
    default focus.

    If `pattern` could not be found, returns None.
    """

    pattern = pattern.casefold()

    def match(f):

        if pattern is None:
            if f.x is None:
                # one focus, at most, ever branches here
                # the main "click to continue" one
                # it's the only one, if any, to be retained in the matching list
                return "default"
            else:
                return None

        if f.x is None:
            t = renpy.display.tts.root._tts_all() # @UndefinedVariable
        else:
            t = f.widget._tts_all()

        if pattern in t.casefold():
            return t
        else:
            return None

    # {focus : (len(alt), alt)}
    matching = {}

    for f in renpy.display.focus.focus_list:

        alt = match(f)

        if alt is not None:
            matching[f] = (len(alt), alt)

    # This gets the matching displayable with the shortest alt text, which
    # is likely what we want.
    return min(matching, key=matching.get, default=None) # type: ignore


def relative_position(x, posx, width):
    if posx is not None:
        if type(posx) is float:
            x = posx * (width - 1)
        else:
            x = posx

    return int(x)


def find_position(f, position):
    """
    Returns the virtual position of a coordinate located within focus `f`.
    If position is (None, None) returns the current mouse position (if in
    the focus), or a random position.

    If `f` is None, returns a position relative to the screen as a whole.
    """

    posx, posy = position

    # Avoid moving the mouse when unnecessary.
    if renpy.test.testmouse.mouse_pos is not None:
        x, y = renpy.test.testmouse.mouse_pos
    else:
        x = random.randrange(renpy.config.screen_width)
        y = random.randrange(renpy.config.screen_height)

    if f is None:
        return (
            relative_position(x, posx, renpy.config.screen_width),
            relative_position(y, posy, renpy.config.screen_height),
            )

    orig_f = f

    # Check for the default widget.
    if f.x is None:
        f = f.copy()
        f.x = 0
        f.y = 0
        f.w = renpy.config.screen_width
        f.h = renpy.config.screen_height

    x = relative_position(x-f.x, posx, f.w) + f.x
    y = relative_position(y-f.y, posy, f.h) + f.y

    for _i in range(renpy.test.testast._test.focus_trials):

        x = int(x)
        y = int(y)

        nf = renpy.display.render.focus_at_point(x, y)

        if nf is None:
            if orig_f.x is None:
                return x, y
        else:
            if (nf.widget == f.widget) and (nf.arg == f.arg):
                return x, y

        x = random.randrange(f.x, f.x + f.w)
        y = random.randrange(f.y, f.y + f.h)

    raise Exception("Could not locate the displayable.")
