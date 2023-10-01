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

# This file contains support for string translation and string formatting
# operations.

from __future__ import division, absolute_import, with_statement, print_function, unicode_literals
from renpy.compat import PY2, basestring, bchr, bord, chr, open, pystr, range, round, str, tobytes, unicode # *



import renpy
import ast
import string
import os
import re

update_translations = "RENPY_UPDATE_TRANSLATIONS" in os.environ


class Formatter(string.Formatter):
    """
    A string formatter that uses Ren'Py's formatting rules. Ren'Py uses
    square brackets to introduce formatting, and it supports a q conversion
    that quotes the text being shown to the user.
    """

    def parse(self, s):
        """
        Parses s according to Ren'Py string formatting rules. Returns a list
        of (literal_text, field_name, format, replacement) tuples, just like
        the method we're overriding.
        """

        # States for the parse state machine.
        LITERAL = 0
        OPEN_BRACKET = 1
        VALUE = 3
        FORMAT = 4
        CONVERSION = 5

        # The depth of brackets we've seen.
        bracket_depth = 0

        # The parts we've seen.
        literal = ''
        value = ''
        format = '' # @ReservedAssignment
        conversion = None

        state = LITERAL

        for c in s:

            if state == LITERAL:
                if c == '[':
                    state = OPEN_BRACKET
                    continue
                else:
                    literal += c
                    continue

            elif state == OPEN_BRACKET:
                if c == '[':
                    literal += c
                    state = LITERAL
                    continue

                else:
                    value = c
                    state = VALUE
                    bracket_depth = 0
                    continue

            elif state == VALUE:

                if c == '[':
                    bracket_depth += 1
                    value += c
                    continue

                elif c == ']':

                    if bracket_depth:
                        bracket_depth -= 1
                        value += c
                        continue

                    else:
                        yield (literal, value, format, conversion)
                        state = LITERAL
                        literal = ''
                        value = ''
                        format = '' # @ReservedAssignment
                        conversion = None
                        continue

                elif c == ':':
                    state = FORMAT
                    continue

                elif c == '!':
                    state = CONVERSION
                    conversion = ''
                    continue

                else:
                    value += c
                    continue

            elif state == FORMAT:

                if c == ']':
                    yield (literal, value, format, conversion)
                    state = LITERAL
                    literal = ''
                    value = ''
                    format = '' # @ReservedAssignment
                    conversion = None
                    continue

                elif c == '!':
                    state = CONVERSION
                    conversion = ''
                    continue

                else:
                    format += c
                    continue

            elif state == CONVERSION:
                if c == ']':
                    yield (literal, value, format, conversion)
                    state = LITERAL
                    literal = ''
                    value = ''
                    format = '' # @ReservedAssignment
                    conversion = None
                    continue

                else:
                    conversion += c
                    continue

        if state != LITERAL:
            raise Exception("String {0!r} ends with an open format operation.".format(s))

        if literal:
            yield (literal, None, None, None)

    def get_field(self, field_name, args, kwargs):
        obj, arg_used = super(Formatter, self).get_field(field_name, args, kwargs)

        return (obj, kwargs), arg_used

    def convert_field(self, value, conversion):
        value, kwargs = value

        if callable(value) and renpy.config.interpolate_calls_functions:
            try:
                value = value()
            except Exception:
                pass

        if conversion is None:
            return value

        if not conversion:
            raise ValueError("Conversion specifier can't be empty.")

        if set(conversion) - set("rstqulci!"):
            raise ValueError("Unknown symbols in conversion specifier, this must use only the \"rstqulci\".")

        if "r" in conversion:
            value = repr(value)
            conversion = conversion.replace("r", "")
        elif "s" in conversion:
            value = str(value)
            conversion = conversion.replace("s", "")

        if not conversion:
            return value

        # All conversion symbols in additional_conversion assume we have a string.
        if not isinstance(value, basestring):
            value = str(value)

        return additional_conversion(value, conversion)


# The instance of Formatter we use.
formatter = Formatter()


# https://docs.python.org/3/library/ast.html#ast.FormattedValue
sra_ref = dict(
    s=115,
    r=114,
    a=97,
)

def fstring_formatter(s, kwargs):
    def ast_replacement(match):
        expr, bangs, suf = match.group(1, 2, 3)
        # expr : the part before the first bang
        # bangs : the bang (if any) and the characters going with it
        # suf : the colon (if any), and the characters going with it

        joinedstr_values = []

        if "=" in expr:
            joinedstr_values.append(ast.Constant(expr))
            expr = expr.partition("=")[0].strip()

        formaval_kw = {
            "value" : ast.parse(expr, mode="eval").body,
            "conversion" : -1,
            "format_spec" : None,
        }

        if bangs:
            bangs = set(bangs)
            bangs.discard("!")

            if not bangs:
                raise Exception("Conversion specifier can't be empty.")

            sra_flags = bangs.intersection("sra")
            if sra_flags:
                bangs -= sra_flags
                sra_flag = sra_flags.pop()
                if sra_flags:
                    raise Exception("Only one of s, r, a flags can be used.")

                formaval_kw["conversion"] = sra_ref[sra_flag]

        if suf:
            formaval_kw["format_spec"] = ast.JoinedStr([ast.Constant(suf)])

        joinedstr_values.append(ast.FormattedValue(**formaval_kw))

        astree = ast.Expression(ast.JoinedStr(joinedstr_values))
        ast.fix_missing_locations(astree)
        value = eval(compile(astree, "<interpolation>", "eval"), {}, kwargs)

        if bangs:
            return additional_conversion(value, bangs)
        return value

    return re.sub(r"\[([^!:\n]+)((?:![^:\s]+)?)\s*(:[^!:\n]+)?\]", ast_replacement, s)

    def patchwork_replacement(match):
        pre, bangs, suf = match.group(1, 2, 3)
        # pre : the part before the first bang
        # bangs : the bang (if any) and the characters going with it
        # suf : potential whitespace, the colon (if any), and the characters going with it

        if '"' in pre:
            begin, end = "f'{", "}'"
        else:
            begin, end = 'f"{', '}"'

        if not bangs:
            return eval("".join((begin, pre, suf, end)), {}, kwargs)

        conversion = set(bangs[1:]) # the first character is always a bang
        sra = conversion - set("tiqulc")
        conversion = conversion - sra
        if sra:
            sra = "!" + "".join(sra)

        value = eval("".join((begin, pre, (sra or ""), suf, end)), {}, kwargs) # type: ignore

        return additional_conversion(value, conversion)

    return re.sub(r"\[([^!:\n]+)((?:![^!:\s]+)?)(\s*(?::[^!:\n]+)?)\]", patchwork_replacement, s)


def additional_conversion(value, conversion):
    if "t" in conversion:
        value = renpy.translation.translate_string(value)

    if "i" in conversion:
        try:
            value = substitute(value, translate=False)[0]
        except RuntimeError: # PY3 RecursionError
            raise ValueError("Substitution {!r} refers to itself in a loop.".format(value))

    if "q" in conversion:
        value = value.replace("{", "{{")

    if "u" in conversion:
        value = value.upper()

    if "l" in conversion:
        value = value.lower()

    if "c" in conversion and value:
        value = value[0].upper() + value[1:]

    return value


class MultipleDict(object):

    def __init__(self, *dicts):
        self.dicts = dicts

    def __getitem__(self, key):
        for d in self.dicts:
            if key in d:
                return d[key]

        raise NameError("Name '{}' is not defined.".format(key))


def substitute(s, scope=None, force=False, translate=True):
    """
    Performs translation and formatting on `s`, as necessary.

    `scope`
        The scope which is used in formatting, in addition to the default
        store.

    `force`
        Force substitution to occur, even if it's disabled in the config.

    `translate`
        Determines if translation occurs.

    Returns the substituted string, and a flag that is True if substitution
    occurred, or False if no substitution occurred.
    """

    if not isinstance(s, basestring):
        s = str(s)

    if translate:
        s = renpy.translation.translate_string(s)

    # Substitute.
    if not renpy.config.new_substitutions and not force:
        return s, False

    if "[" not in s:
        return s, False

    old_s = s

    if scope is not None:
        kwargs = MultipleDict(scope, renpy.store.__dict__) # @UndefinedVariable
    else:
        kwargs = renpy.store.__dict__ # @UndefinedVariable

    try:
        if renpy.config.fstring_substitute:
            s = fstring_formatter(s, kwargs)
        else:
            s = formatter.vformat(s, (), kwargs) # type: ignore
    except Exception:
        if renpy.display.predict.predicting: # @UndefinedVariable
            return " ", True
        raise

    return s, (s != old_s)
