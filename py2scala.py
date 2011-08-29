#!/usr/bin/python

# Copyright (C) 2006-2011 Ben Wing <ben@benwing.com>

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * The name of Ben Wing may not be used to endorse or promote products
#   derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import re
import sys
import fileinput
import optparse

###########################################################################
#
# Command-line options and usage
#
###########################################################################

usage = """%prog [OPTIONS] [FILE]

Convert a Python file to Scala.

Conversion is not perfect, and it is completely expected that further
manual editing is required.  The idea is to take care of the most common
conversions necessary, especially the ones (like adding braces) that are
difficult to do with a simple search/replace.

An important property of this program is that it is (largely) idempotent,
i.e. if you run it on code that has previously been converted, it should
do nothing, and if run on partially converted code it should only convert
the remainder.  This is useful for various reasons.  One of them has to
do with conversions like removing `self.' or changing bracketed array
references to parens that remove useful info.  Eventually we want to make
these changes, but before then we may find the unchanged code useful for
e.g. redoing class constructors (which are done in a completely different
fashion in Python and Scala, and basically need to be converted manually)
and adding type annotations to functions (again something to do manually).
Thus, the program is intended to work as follows:

1. Run it to do a preliminary conversion
2. Fix up class constructors, add type annotations
3. Run it again with appropriate options to fix up self references and brackets

Currently, parsing is done with regexps rather than context-free.  This means
that some constructions will not be converted perfectly.  Currently the
program's handling of strings and one-line comments is rather good, and
its handling of multiline strings decent, but it doesn't yet properly handle
if statements and other block openers that stretch across multiple lines.
(These will be left alone.)
"""

parser = optparse.OptionParser(usage=usage)
parser.add_option("-s", "--remove-self", action="store_true",
                   help="""Remove self.* references and self params.
Also removes cls.* references and cls params.   Not done by default because
it removes info necessary for manually converting classes (especially
constructors) and separating class methods into companion objects.  Useful
to rerun this program with this option after these things have been done.""")
parser.add_option("-b", "--convert-brackets", action="store_true",
                   help="""Try to convert array refs like foo[i] to Scala-style foo(i).
Not done by default because it removes info useful for adding type annotations
to functions.  Useful to rerun this program with this option after doing
this.  You will still have to convert slices by hand.  This attempts not
to convert bracket references that are meaningful to Scala (i.e. generic
type parameters) using the assumption that types in Scala begin with an
uppercase letter.""")

(options, args) = parser.parse_args()

debug = True

## Process file(s)

def uniprint(text, outfile=sys.stdout, nonl=False, flush=False):
  '''Print text string using 'print', converting Unicode as necessary.
If string is Unicode, automatically convert to UTF-8, so it can be output
without errors.  Send output to the file given in OUTFILE (default is
stdout).  Uses the 'print' command, and normally outputs a newline; but
this can be suppressed using NONL.  Output is not normally flushed (unless
the stream does this automatically); but this can be forced using FLUSH.'''
  
  if type(text) is unicode:
    text = text.encode("utf-8")
  if nonl:
    print >>outfile, text,
  else:
    print >>outfile, text
  if flush:
    outfile.flush()

def errprint(text, nonl=False):
  '''Print text to stderr using 'print', converting Unicode as necessary.
If string is Unicode, automatically convert to UTF-8, so it can be output
without errors.  Uses the 'print' command, and normally outputs a newline; but
this can be suppressed using NONL.'''
  uniprint(text, outfile=sys.stderr, nonl=nonl)

def debprint(fmt, *vals):
  if debug:
    errprint("Debug: %s" % (fmt % vals))

lines = []

balparenexpr = r'\([^()]*\)'
balbracketexpr = r'\[[^\[\]]*\]'
balstr0 = r'(?:[^()\[\]]|%s|%s)*' % (balparenexpr, balbracketexpr)
bal2parenexpr = r'\(%s\)' % balstr0
bal2bracketexpr = r'\[%s\]' % balstr0
bal2str0 = r'(?:[^()\[\]]|%s|%s)*' % (bal2parenexpr, bal2bracketexpr)
bal2strnospace0 = r'(?:[^ ()\[\]]|%s|%s)*' % (bal2parenexpr, bal2bracketexpr)
bal2str = r'(?:[^()\[\]]|%s|%s)+' % (bal2parenexpr, bal2bracketexpr)
bal2strnospace = r'(?:[^ ()\[\]]|%s|%s)+' % (bal2parenexpr, bal2bracketexpr)

stringre = re.compile(r'''(   r?'[']' .*? '[']'   # 3-single-quoted string
                            | r?""" .*? """       # 3-double-quoted string 
                            | r?'[']'.*           # unmatched 3-single-quote
                            | r?""".*             # unmatched 3-double-quote
                            | r?' (?:\\.|[^'])* ' # single-quoted string
                            | r?" (?:\\.|[^"])* " # double-quoted string
                            | r?'.*               # unmatched single-quote
                            | r?".*               # unmatched double-quote
                            | [#].*               # Python comment
                            | //.*                # Scala comment
                          )''', re.X)
def teststr(x):
  split = stringre.split(x)
  for y in split:
    print y

triple_quote_delims = ['"""', "'''"]
single_quote_delims = ['"', "'"]
# Main function to frob the inside of a line.  Written as a generator
# function using `yield'.
def modline(split):
  for i in xrange(len(split)):
    prev = None
    if i > 0:
      prev = split[i-1]
    vv = split[i]
    #debprint("Saw #%d: %s", i, vv)
    if len(vv) == 0:
      continue
    if i % 2 == 1:
      # We are looking at a delimiter

      # Look for raw-string prefix
      vv2 = vv
      raw = None
      if vv[0] == 'r' and len(vv) > 1 and vv[1] in single_quote_delims:
        vv2 = vv[1:]
        raw = "r"
      elif vv[0] in single_quote_delims:
        raw = ""
      if raw is not None:
        # We're handline some sort of string
        # Look for (unclosed) triple quote
        triplequote = False
        unclosed = False
        for delim in triple_quote_delims:
          if vv2.startswith(delim):
            triplequote = True
            if not vv2.endswith(delim):
              global openquote
              openquote = delim
              unclosed = True
        if triplequote:
          # FIXME!! Python has eight types of strings: Single and triple
          # quoted strings, using both single and double quotes (i.e.
          # ', ", ''', """), as well as the "raw" variants prefixed with r.
          # Scala has only two types: Strings quoted like "foo", and
          # raw multiline strings quoted like """foo""".  We're not properly
          # shoehorning the various types of Python strings into Scala
          # strings.  The only thing we do is try to convert Python strings
          # like r"foo" and r'foo' into Scala raw """foo""" strings.
          #
          # Note that Scala also has single-character literals like 'f',
          # whereas Python uses normal strings for this.  We try to do the
          # right thing here (i.e. leave 'f' as 'f' but convert 'ff' to "ff"),
          # but we can't be perfect because (a) we don't know whether a
          # single-character Python string should become a Scala string or
          # character literal, and (b) since we can be run more than once,
          # we can't distinguish "f" as a Scala single-character string
          # (should be left alone) from "f" as a Python single-character
          # string (potentially convertible to Scala 'f').
          if vv2.startswith("'''"):
            if unclosed:
              yield raw + '"""' + vv2[3:]
            else:
              yield raw + '"""' + vv2[3:-3] + '"""'
          else:
            yield vv
          continue
        for delim in single_quote_delims:
          if (vv2.startswith(delim) and
              (len(vv2) == 1 or not vv2.endswith(delim))):
            warning("Saw unfinished single quoted string %s" % vv)
            yield vv
            continue
        revisedstr = vv2
        if vv2.startswith("'") and (
            (len(vv2) != 4 if not raw and vv2[1] == '\\' else len(vv2) != 3)
            ):
          # Single-quoted string of length != 1
          # Convert to double-quoted string
          revisedstr = '"' + vv2[1:-1] + '"'
        # FIXME! This may fail with unbackslashed quotes of the other
        # sort in the string.  See comments above about the eight types of
        # Python strings.
        if raw:
          yield '""' + revisedstr + '""'
        else:
          yield revisedstr
        continue
        # We don't convert in the opposite direction because in Scala
        # someone might reasonably have a double-quoted string of length 1

      # Convert comments
      if vv.startswith('#'):
        yield '//' + vv[1:]
        continue
      yield vv

    else:
      # Not a delimiter

      global paren_mismatch
      paren_mismatch += vv.count('(') - vv.count(')')

      vv = re.sub(r'\bor\b', '||', vv)
      vv = re.sub(r'\band\b', '&&', vv)
      vv = re.sub(r'\bTrue\b', 'true', vv)
      vv = re.sub(r'\bFalse\b', 'false', vv)
      # Next one maybe questionable; some None should actually be None
      # (e.g. when Option[Int] is used)
      vv = re.sub(r'\bNone\b', 'null', vv)
      vv = re.sub(r'\bnot ', '!', vv)
      vv = re.sub(r'\bis (None|null)\b', '== null', vv)
      vv = re.sub(r'\bis !.*(None|null)\b', '!= null', vv)
      vv = re.sub(r'lambda ([A-Za-z0-9]+):', r'\1 =>', vv)
      # Seems this isn't necessary; (for x <- y if foo) works fine in Scala
      #vv = re.sub(r'[\[(](.*) for (.*) in (.*) if (.*)[)\]]',
      #             r'(for (\2 <- \3; if \4) yield \1)', vv)
      vv = re.sub(r'[\[(](%s) for (.*) in (%s)[)\]]' % (bal2str, bal2str),
                   r'(for (\2 <- \3) yield \1)', vv)
      if not re.match(r'.*\bfor\b', vv):
        vv = re.sub(r'(%s) in (%s)\b' % (bal2strnospace, bal2strnospace),
            r'\2 contains \1', vv)
      vv = re.sub(r'len\((%s)\)' % bal2str, r'\1.length', vv)
      vv = re.sub(r'\bpass\b', '()', vv)
      # change % to format but only when applied to string
      if prev and prev[0] in single_quote_delims:
        vv = re.sub(r'^( +)%( +)', r'\1format\2', vv)
      if options.remove_self:
        vv = re.sub(r'\bself\.', '', vv)
        vv = re.sub(r'\bself\b', 'this', vv)
        vv = re.sub(r'\bcls\.', '', vv)
        # Not sure about this
        #vv = re.sub(r'\bcls\b', 'this', vv)
      if options.convert_brackets:
        # Convert bracketed expressions, but avoid list constructors
        # (previous character not alphanumeric) and scala generic vars/types
        # (the type in brackets is usually uppercase)
        vv = re.sub(r'([A-Za-z0-9_])\[([^A-Z\]]%s)\]' % bal2str0,
            r'\1(\2)', vv)

      yield vv

curindent = 0
contline = None
# Unclosed multi-line quote to check for (''' or """)
openquote = None
# Mismatch in parens so far
paren_mismatch = 0
# Indent last time paren mismatch was zero
zero_mismatch_indent = 0
# Line # last time paren mismatch was zero
zero_mismatch_lineno = 0
# List of tuples (LINE#, INDENT, TYPE) of indentation of blocks
# (if, for, else, ...), where TYPE is "python" or "scala"
indents = []
# For each currently active function define, list of tuples of indent level
# and list of currently active params and local vars
defs = []
# Current line number
lineno = 0

def warning(text, nonl=False):
  '''Line errprint() but also add "Warning: " and line# to the beginning.'''
  errprint("Warning: %d: %s" % (lineno, text), nonl=nonl)

for line in fileinput.input(args):
  lineno += 1
  newblock = None

  line = line.rstrip("\r\n").expandtabs()
  #debprint("Saw line: %s", line)
  # If previous line was continued, add it to this line
  if contline:
    line = contline.rstrip() + " " + line.lstrip()
    contline = None
                                  
  old_paren_mismatch = paren_mismatch
  oldline = line

  # If line is continued, don't do anything yet (till we get the whole line)
  if line and line[-1] == '\\':
    contline = line[0:-1]
    continue
  # Ignore blank line for purposes of figuring out indentation
  # NOTE: No need to use \s* in these or other regexps because we call
  # expandtabs() above to convert tabs to spaces, and rstrip() above to
  # remove \r and \n
  if re.match(r'^ *$', line):
    lines += [line]
    continue
  # If we are in the middle of a multi-line quote, ignore for purposes of
  # figuring out indentation; also check for end of quote
  if openquote:
    # FIXME: Won't correctly handle start of another multi-line quote on
    # same line
    splitvals = line.split(openquote)
    if len(splitvals) > 1:
      for split in splitvals[1:]:
        paren_mismatch += split.count('(') - split.count(')')
      openquote = None
    lines += [line]
    continue

  if options.remove_self:
    m = re.match(r'^( *def +[A-Za-z0-9_]+)\((?:self|cls)(\)|, *)(.*)$', line)
    if m:
      if m.group(2) == ')':
        line = '%s()%s' % (m.group(1), m.group(3))
      else:
        line = '%s(%s' % (m.group(1), m.group(3))
    if re.match(r'^ *def +__init__\(', line):
      warning("Need to convert to Scala constructor: %s" % line)

  #line = ''.join(modline([line]))
  line = ''.join(modline(stringre.split(line)))

  #debprint("Line %d, old paren mismatch %d, new paren mismatch %d",
  #    lineno, old_paren_mismatch, paren_mismatch)
  if paren_mismatch < 0:
    warning("Apparent unmatched right-paren, we might be confused: %s" % line)
    paren_mismatch = 0

  # Get current indentation
  m = re.match('( *)', line)
  indent = len(m.group(1))
  if old_paren_mismatch == 0:
    zero_mismatch_indent = indent
    zero_mismatch_lineno = lineno
  if indent < curindent:
    # Pop off all indentation blocks at or more indented than current
    # position, and add right braces
    while indents and indents[-1][1] >= indent:
      blockline, blockind, blocktype = indents.pop()
      # Can happen, e.g., if // is used in Python to mean "integer division",
      # or other circumstances where we got confused
      if old_paren_mismatch > 0:
        warning("Apparent unmatched left-paren somewhere before, possibly line %d, we might be confused" % zero_mismatch_lineno)
        # Reset to only mismatched left parens on this line
        paren_mismatch = paren_mismatch - old_paren_mismatch
        if paren_mismatch < 0:
          paren_mismatch = 0
      if blocktype == "scala":
        continue
      rbrace = "%s}" % (' '*blockind)
      # Check for right brace already present; if so, just make sure
      # corresponding left brace is present
      if line.startswith(rbrace):
        lines[blockline] += " {"
      else:
        insertpos = len(lines)
        # Insert the right brace *before* any blank lines (we skipped over
        # them since they don't affect indentation)
        while re.match('^ *$', lines[insertpos - 1]):
          insertpos -= 1
        # If the "block" is only a single line, and it's not introduced
        # by "def" or "class", don't add braces.
        # We check for 2 because with a single-line block, the potential
        # right-brace insertion point is 2 lines past the opening block
        # (1 for opening line itself, 1 for block)
        if (insertpos - blockline > 2 or
            re.match('^ *(def|class) ', lines[blockline])):
          lines[blockline] += " {"
          lines[insertpos:insertpos] = [rbrace]
    # Pop off all function definitions that have been closed
    while defs and defs[-1][0] >= indent:
      defs.pop()
  curindent = indent

  # Handle blocks.  We only want to check once per line, and Python
  # unfortunately makes it rather awkward to do convenient if-then checks
  # with regexps because there's no equivalent of
  #
  # if ((m = re.match(...))):
  #   do something with m.groups()
  # elif ((m = re.match(...))):
  #   etc.
  #
  # Instead you need assignment and check on separate lines, and so all
  # ways of chaining multiple regex matches will be awkward.  We choose
  # to create an infinite loop and break after each match, or at the end.
  # This almost directly mirrors the architecture of a C switch() statement.
  #

  # If we see a Scala-style indent, just note it; important for unmatched-
  # paren handling above.
  if paren_mismatch == 0 and re.match(r'.*\{ *$', line):
    indents += [(len(lines), zero_mismatch_indent, "scala")]
  front, body, back = "", "", ""
  frontbody = line
  # Look for a statement introducing a body
  m = re.match(r'( *)(.*?) *: *$', frontbody)
  if m:
    front, body = m.groups()
  else:
    splits = re.split(r'(#|//)', line, 1)
    if len(splits) == 3:
      frontbody = splits[0]
      newback = splits[1] + splits[2]
      m = re.match(r'''( *)([^\'\"]*?) *: *$''', frontbody)
      if m:
        front, body = m.groups()
        back = " " + newback

  # Check for def/class and note function arguments.  We do this separately
  # from the def check below so we find both def and class, and both
  # Scala and Python style.
  m = re.match(' *(?:def|class) +(.*?)\((.*)\) *(: *$|=? *\{ *$|extends .*|with .*| *$)', line)
  if m:
    allargs = m.group(2).strip()
    argdict = {}
    if allargs:
      args = allargs.split(',')
      # Strip off default assignments
      args = [x.strip().split('=')[0].strip() for x in args]
      # Strip off Scala types
      args = [x.strip().split(':')[0].strip() for x in args]
      for arg in args:
        if arg.startswith("var "):
          argdict[arg[4:].strip()] = "var"
        elif arg.startswith("val "):
          argdict[arg[4:].strip()] = "val"
        else:
          argdict[arg] = "val"
    defs += [(indent, argdict)]
    #debprint("Adding args %s for function", argdict)

  while True:
    if body:
      # Check for def
      m = re.match('def +(.*?)\((.*)\)$', body)
      if m:
        newblock = "def %s(%s)" % m.groups()
        break
      # Check for 'for' statement
      m = re.match('for +(.*?) +in +(.*)$', body)
      if m:
        newblock = "for (%s <- %s)" % m.groups()
        break
      # Check for 'if' statement
      m = re.match('if +(.*)$', body)
      if m:
        newblock = "if (%s)" % m.groups()
        break
      # Check for 'elif' statement
      m = re.match('elif +(.*)$', body)
      if m:
        newblock = "else if (%s)" % m.groups()
        break
      # Check for 'else' statement
      m = re.match('else *$', body)
      if m:
        newblock = "else"
        break
      # Check for 'while' statement
      m = re.match('while (.*)$', body)
      if m:
        newblock = "while (%s)" % m.groups()
        break
      # Check for 'try' statement
      m = re.match('try *$', body)
      if m:
        newblock = "try"
        break
      # Check for bare 'except' statement
      m = re.match('except *$', body)
      if m:
        newblock = "catch"
        break
      # Check for 'except' statement
      # FIXME: Should convert to a case statement within the body
      m = re.match('except (.*)$', body)
      if m:
        newblock = "catch %s" % m.groups()
        break
      # Check for 'finally' statement
      m = re.match('finally *$', body)
      if m:
        newblock = "finally"
        break
      # Check for 'class(object)' statement
      # Class that inherits from `object' (new-style class), convert to
      # class without superclass
      m = re.match('class (.*)\(object\)', body)
      if m:
        newblock = "class %s" % m.groups()
        break
      # Check for 'class(superclass)' statement
      m = re.match('class (.*)\((.*)\)$', body)
      if m:
        newblock = "class %s extends %s" % m.groups()
        break
      # Check for 'class' statement (no superclass)
      m = re.match('class ([^(]*)$', body)
      if m:
        newblock = "class %s" % m.groups()
        break
    # Check for assignments to variables inside of functions.  Add val/var
    # to bare assignments to variables not yet seen.  If reassigning a
    # variable, and we previously added a val, change it to var.  Don't
    # do this if we're inside of a function call or similar (where the
    # use of a named param looks like a variable assignment).
    #debprint("About to check for vars, line %d, old paren mismatch %d",
    #    lineno, old_paren_mismatch)
    if defs and old_paren_mismatch == 0:
      curvardict = defs[-1][1]
      # Make sure we see a variable assignment in both the unfrobbed and
      # frobbed lines, so that we ignore cases where we removed a `self.'.
      m = re.match('( *)(val |var |)([a-zA-Z_][a-zA-Z_0-9]*)( *=)(.*)', oldline)
      if m:
        m = re.match('( *)(val |var |)([a-zA-Z_][a-zA-Z_0-9]*)( *=)(.*)', line)
      if m:
        newvar = m.group(3)
        if m.group(2):
          # Already seen val/var decl
          if newvar in curvardict:
            warning("Apparent redefinition of variable %s" % newvar)
          else:
            # Signal not to try and change val to var
            curvardict[newvar] = "explicit"
        else:
          #debprint("newvar: %s, curvardict: %s", newvar, curvardict)
          if newvar not in curvardict:
            curvardict[newvar] = len(lines)
            line = "%sval %s%s%s%s" % m.groups()
          else:
            vardefline = curvardict[newvar]
            if vardefline == "val":
              warning("Attempt to set function parameter %s" % newvar)
            elif type(vardefline) is int:
              lines[vardefline] = re.sub(r'^( *)val ', r'\1var ',
                lines[vardefline])
        break
    break
  if newblock:
    indents += [(len(lines), indent, "python")]
    lines += [front + newblock + back]
  else:
    lines += [line]

for line in lines:
  print line
