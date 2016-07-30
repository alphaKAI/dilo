/*
 *   Dilo - A minimal Text Editor in D Language, ported from Kilo.
 *
 *                    A minimal Text Editor - Dilo
 *
 *  Dilo is a minimal text Editor in D Language, ported from Kilo.
 * As original Kilo, currently, doesn't depend on libcurses.
 * Current version of Dilo is meraly ported, but I'll imporove.
 *
 * Features:
 *  - Text Editing
 *  - Syntax Highlight (this version supports C/C++ only, but you can add others language)
 *  - Incremental Search
 *
 * Implementation/improving plans:
 *  - Separate codes to some files, for this program is distributed as single-file
 *  - Support syntax highlight definition with external file.
 *  - Support sinippets/completion
 *
 *
 * Most of original comments are kept, but some of them are saved.
 * I'll write alternative comments to make up it.
 *
 * Copyright (C) 2016 Akihiro Shoji <alpha.kai.net at alpha-kai-net.info>
 *
 */

// Original Kilo's copyright and disclaimers:

/* Kilo -- A very simple editor in less than 1-kilo lines of code (as counted
 *         by "cloc"). Does not depend on libcurses, directly emits VT100
 *         escapes on the terminal.
 *
 * -----------------------------------------------------------------------
 *
 * Copyright (C) 2016 Salvatore Sanfilippo <antirez at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

enum DILO_VERSION = "0.0.1";

import core.sys.posix.sys.types,
       core.sys.posix.sys.ioctl,
       core.sys.posix.sys.time,
       core.sys.posix.termios,
       core.sys.posix.unistd,
       core.sys.posix.fcntl,
       core.stdc.string,
       core.stdc.stdlib,
       core.stdc.ctype,
       core.stdc.errno;
import core.memory,
       core.vararg;
import std.algorithm,
       std.format,
       std.string,
       std.array,
       std.stdio,
       std.conv,
       std.file,
       std.path;

/* Memory Management Utilities */
T malloc(T)(size_t size, uint flags = 0u) {
  // use calloc instead of malloc implicitly
  return cast(T)GC.calloc(size, flags);
}

T realloc(T)(T ptr, size_t size) {
  return cast(T)GC.realloc(ptr, size);
}

version (OSX) {
  // Special code for OS X.
  // Currently, core.sys.posix.sys.termios doesn't contain constant `TIOCGWINSZ` for OSX.
  enum TIOCGWINSZ = 0x40087468;
}

/* Syntax Highlight types */
enum HL {
  NORMAL,
  NONPRINT,
  COMMENT,   /* Single line comment. */
  MLCOMMENT, /* Multi line comment. */
  KEYWORD1,
  KEYWORD2,
  STRING,
  NUMBER,
  MATCH /* Search match. */
}

enum USE_SPACE_INSTADE_OF_TAB = true;
enum TAB_SPACE_SIZE           = 2;

struct EditorSyntax {
  string   syntaxName;
  string[] filematch,
           keywords;
  string   singleline_comment_start,
           multiline_comment_start,
           multiline_comment_end;
}

/* This structure represents a single line of the file we are editing. */
struct Erow {
  ulong   idx,    /* Row index in the file, zero-based. */
          size,   /* Size of the row, exculuding the null term. */
          rsize;  /* Size of the rendered row.  */
  char*   chars;  /* Row content. */
  char*   render; /* Row content "rendered" for screen (for TABs). */
  ubyte*  hl;     /* Syntax highlight type for each character in render. */
  bool    hl_oc;  /* Row had open comment at end in last syntax highlight check. */
}

struct EditorConfig {
  ulong  cx, cy;      /* Cursor x and y position in characters. */
  ulong  rowoff,      /* Offset of row displayed. */
         coloff;      /* Offset of column displayed. */
  int    screenrows,  /* Number of rows that we can show. */
         screencols,  /* Number of cols that we can show. */
         numrows;     /* Number of rows. */
  bool   rawmode;     /* Is terminal raw mode enabled? */
  Erow*  row;         /* Rows */
  bool   dirty;       /* File modified but not saved. */
  string filename;    /* Currently open filename */
  Appender!string statusmsg;
  time_t          statusmsg_time;
  EditorSyntax*   syntax;       /* Current syntax highlight, or NULL. */
  termios         orig_termios; /* In order to restore at exit. */
}

static EditorConfig E;

enum KEY_ACTION {
  KEY_NULL     = 0,       /* NULL */
  CTRL_C       = 3,       /* Ctrl-c */
  CTRL_D       = 4,       /* Ctrl-d */
  CTRL_F       = 6,       /* Ctrl-f */
  CTRL_H       = 8,       /* Ctrl-h */
  TAB          = 9,       /* Tab */
  CTRL_L       = 12,      /* Ctrl+l */
  ENTER        = 13,      /* Enter */
  CTRL_Q       = 17,      /* Ctrl-q */
  CTRL_S       = 19,      /* Ctrl-s */
  CTRL_U       = 21,      /* Ctrl-u */
  ESC          = 27,      /* Escape */
  BACKSPACE    = 127,    /* Backspace */
  /* The following are just soft codes, not really reported by the terminal directly. */
  ARROW_LEFT   = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  DEL_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN
}

/*
  Syntax highlights

  In order to add a new syntax, define two arrays as follows declaration of C/C++.

  Currently, supports two colors.
  If '|' is added at the end of keyword, the keyword will be colored in a different color.
  "foo"  <- pattern 1
  "bar|" <- pattern 2
 */

enum string[] C_HL_extensions = [".c", ".cpp"];
enum string[] C_HL_keywords   = [
  /* A few C / C++ keywords */
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class",
  /* C types */
  "int|","long|","double|","float|","char|","unsigned|","signed|",
  "void|"
];

enum string[] D_HL_extensions = [".d"];
enum string[] D_HL_keywords   = [
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "class", "static", "enum", "alias", "mixin", "template",
  "final", "do", "foreach", "scope", "in", "body", "out", "const", "immutable",
  "delegate", "function", "version", "is", "typeof", "typeid", "with", "import",
  "byte|", "ubyte|", "short|", "ushort|", "int|", "uint|", "long|", "ulong|",
  "string|", "wstring|", "dstring|", "char|", "wchar|", "dchar|", "float|", "double|", "real|", "void|"
];

/* Here we define an array of syntax highlights by extensions, keywords, comments delimiters and flags */
enum static EditorSyntax[] HLDB = [
  {
    "C/C++",
    C_HL_extensions,
    C_HL_keywords,
    "//",
    "/*",
    "*/"
  },
  {
    "D",
    D_HL_extensions,
    D_HL_keywords,
    "//",
    "/*",
    "*/"
  }
];

enum HLDB_ENTRIES = HLDB.length;

/* Low Level terminal handling */

void disableRawMode(int fd) {
  /* Don't even check the return value as it's too late. */
  if (E.rawmode) {
    tcsetattr(fd, TCSAFLUSH, &E.orig_termios);
    E.rawmode = false;
  }
}

/* Called at Exit to avoid remaining in raw mode. */
extern (C) void editorAtExit() {
  disableRawMode(STDIN_FILENO);
  write("\x1B[2J\x1b[1;1H");
}

/* Raw mode: 1960 magic shit. */
int enableRawMode(int fd) {
  termios raw;

  if (E.rawmode) {
    return 0; /* Already enabled. */
  }

  if (!isatty(STDIN_FILENO)) {
    goto fatal;
  }

  // set callback
  atexit(&editorAtExit);
  
  if (tcgetattr(fd, &E.orig_termios) == -1) {
    goto fatal;
  }

  raw = E.orig_termios; /* modify the original mode */
  /* input modes: no break, no CT to NL, no parity check, no strip char, no start/stop output control. */
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  /* output modes - disable post processiong */
  raw.c_oflag &= ~(OPOST);
  /* control mode - set 8 bit chars */
  raw.c_cflag |= CS8;
  /* local modes - echoing off, canonical off, no extended function, no signal chars (^Z, ^C) */
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  /* control chars - set return condition: min number of bytes and timer. */
  raw.c_cc[VMIN]  = 0; /* Return each byte. or zero for timeout. */
  raw.c_cc[VTIME] = 1; /* 100ms timeout (unit is tens of second).*/

  /* put terminal in raw mode after flushing */
  if (tcsetattr(fd, TCSAFLUSH, &raw) < 0) {
    goto fatal;
  }

  E.rawmode = true;

  return 0;

fatal:
  errno = ENOTTY;
  return -1;
}

/* Read a key from the terminal put in raw mode, trying to handle escape sequence. */
int editorReadKey(int fd) {
  long nread;
  char c;
  char[3] seq;

  while ((nread = read(fd, &c, 1)) == 0) {}
  if (nread == -1) exit(1);

  while (true) {
    with (KEY_ACTION) switch (c) {
      case ESC: /* excape sequence */
        /* If this is just an ESC, we'll timeout here/ */
        if (read(fd, seq.ptr, 1) == 0) {
          return ESC;
        }
        
        if (read(fd, seq.ptr + 1, 1) == 0) {
          return ESC;
        }

        /* ESC [ sequence */
        if (seq[0] == '[') {
          if (0 <= seq[1] && seq[1] <= '9') {
            /* Extended escape, read additional byte */
            if (read(fd, seq.ptr + 2, 1) == 0) {
              return ESC;
            }

            if (seq[2] == '~') {
              switch (seq[1]) {
                case '3': return DEL_KEY;
                case '5': return PAGE_UP;
                case '6': return PAGE_DOWN;
                default: break;
              }
            }
          } else {
            switch (seq[1]) {
              case 'A': return ARROW_UP;
              case 'B': return ARROW_DOWN;
              case 'C': return ARROW_RIGHT;
              case 'D': return ARROW_LEFT;
              case 'H': return HOME_KEY;
              case 'F': return END_KEY;
              default: break;
            }
          }
        } else if (seq[0] == 'O') { /* ESC O sequence */
          switch (seq[1]) {
            case 'H': return HOME_KEY;
            case 'F': return END_KEY;
            default: break;
          }
        }
        break;
      default:
        return c;
    }
  }
}

/* USE the ESC [6n escape sequence to query horizontal cursor position and return it.
 * On error -1 is returned, on success the position of the cursor is stored at *rows and *cols and 0 is returned. */
int getCursorPosition(int ifd, int ofd, int* rows, int* cols) {
  char[32] buf;
  uint i;

  if (core.sys.posix.unistd.write(ofd, "\x1b[6n".toStringz, 4) != 4) return -1;

  /* Read the response: ESC [ rows; cols R */
  while (i < buf.sizeof - 1) {
    if (read(ifd, buf.ptr + i, 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }

  buf[i] = '\0';

  /* Parse it. */
  if (buf[0] != KEY_ACTION.ESC || buf[1] != '[') return -1;
  if (sscanf(buf.ptr + 2, "%d;%d", rows, cols) != 2) return -1;

  return 0;
}

/* Try to get the number of columns in the Current terminal.
 * If the ioctl() call fails the function will try to query the ternimal itself.
 * Return 0 on success, -1 on error. */
int getWindowSize(int ifd, int ofd, int* rows, int* cols) {
  winsize ws;

  if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    /* ioctl() failed. Try to query the terminal itself.  */
    int orig_row, orig_col, retval;

    /* Get the initial position so we can restore it later. */
    retval = getCursorPosition(ifd, ofd, &orig_row, &orig_col);

    if (retval == -1) {
      goto failed;
    }

    /* Go to right/bottom margin and get position*/
    if (core.sys.posix.unistd.write(ofd, "\x1b[999C\x1b[999B".toStringz, 12) != 12) {
      goto failed;
    }

    retval = getCursorPosition(ifd, ofd, rows, cols);

    if (retval == -1) {
      goto failed;
    }

    /* Restore position. */
    auto seq = appender!string;
    formattedWrite(seq, "\x1b[%d;%dH", orig_row, orig_col);

    if (core.sys.posix.unistd.write(ofd, seq.data.toStringz, seq.data.length) == -1) {
      /* Can't recover... */
    }
    return 0;
  } else {
    *rows = ws.ws_row;
    *cols = ws.ws_col;
    return 0;
  }

failed:
  return -1;
}

/* Syntax highlight color scheme */
bool is_separator(int c) {
  return c == '\0' || c == ' ' || "{},.()+-/*=~%[];".canFind(c);
}

/* Return true if the specified row last char is part of multi line comment
 * that starts at this row or at one before, and does not end at the end 
 * of the row but spawns to the next row. */
bool editorRowHasOpenComment(Erow* row) {
  if (E.syntax is null) {
    return false;
  }

  char mcs1 = E.syntax.multiline_comment_start[0],
       mcs2 = E.syntax.multiline_comment_start[1];

  if (row.hl && row.rsize && row.hl[row.rsize - 1] == HL.MLCOMMENT &&
      (row.rsize < 2 || (row.render[row.rsize - 2] != mcs2 ||
                         row.render[row.rsize - 1] != mcs1))) {
    return true;
  }

  return false;
}

/* Set every byte of row.hl (that corresponds to every character in the line)
 * to the right syntax highlight type (HL.* defines). */
/* Keeping in_comment flag and in_string flag over rows. */
bool prev_sep,
     in_string,  /* Are we inside "" or '' ? */
     in_comment; /* Are we inside multi-line comment? */

void editorUpdateSyntax(Erow* row) {
  row.hl = realloc(row.hl, row.rsize);
  memset(row.hl, HL.NORMAL, row.rsize);

  if (E.syntax is null) {
    return; /* No syntax, eveything is HL.NORMAL */
  }

  ulong    i;
  char*    p;
  string[] keywords = E.syntax.keywords.dup;
  string   scs      = E.syntax.singleline_comment_start,
           mcs      = E.syntax.multiline_comment_start,
           mce      = E.syntax.multiline_comment_end;

  /* Point to the first non-space char. */
  p = row.render;

  while (*p && isspace(*p)) {
    p++;
    i++;
  }

  prev_sep = true;  /* Tell the parse if 'i' points to start of word */

  char string_char;   /* This variable represents kind of the string type: "" or '' if in a string. */

  /* If the previous line has an open comment, this line starts with an open comment state. */
  if (row.idx > 0 && editorRowHasOpenComment(&E.row[row.idx - 1])) {
    in_comment = true;
  }

  while (*p) {
    if (!in_string) {
      /* Handle single-line comments */
      if (prev_sep && *p == scs[0] && *(p + 1) == scs[1]) {
        memset(row.hl+i, HL.COMMENT, row.rsize-i);
        return;
      }

      /* Handle multi-line comments */
      if (in_comment) {
        row.hl[i] = HL.MLCOMMENT;

        if (*p == mce[0] && *(p + 1) == mce[1]) {
          row.hl[i + 1] = HL.MLCOMMENT;
          in_comment    = false;
          prev_sep      = true;
          p += 2;
          i += 2;
          continue;
        } else {
          prev_sep = false;
          p++;
          i++;
          continue;
        }
      } else if (*p == mcs[0] && *(p + 1) == mcs[1]) {
        row.hl[i]     = HL.MLCOMMENT;
        row.hl[i + 1] = HL.MLCOMMENT;
        in_comment    = true;
        prev_sep      = false;
        p += 2;
        i += 2;
        continue;
      }
    }

    if (!in_comment) {
      /* Handle "" and '' */
      if (in_string) {
        row.hl[i] = HL.STRING;

        if (*p == '\\') {
          row.hl[i + 1] = HL.STRING;
          prev_sep      = false;
          p += 2;
          i += 2;
          continue;
        }

        if (*p == string_char) {
          in_string = false;
        }

        p++;
        i++;

        continue;
      } else {
        if (*p == '"' || *p == '\'') {
          string_char = *p;
          in_string   = true;
          row.hl[i]   = HL.STRING;
          prev_sep    = false;
          p++;
          i++;
          continue;
        }
      }
    }

    /* Handle non printable chars */
    /* Needs to be fixed: Currently doesn't support multibyte characters due to next codes: */
    if (!isprint(*p)) {
      row.hl[i] = HL.NONPRINT;
      p++;
      i++;
      prev_sep = false;
      continue;
    }

    /* Handle numbers */
    if ((isdigit(*p) && (prev_sep || row.hl[i - 1] == HL.NUMBER)) ||
        (*p == '.' && i > 0 && row.hl[i - 1] == HL.NUMBER)) {
      row.hl[i] = HL.NUMBER;
      p++;
      i++;
      prev_sep = false;
      continue;
    }

    /* Handle keywords and lib calls */
    if (prev_sep) {
      ulong j;

      for (; j < keywords.length; j++) {
        ulong klen = keywords[j].length;
        bool  kw2  = keywords[j][klen - 1] == '|';

        if (kw2) {
          klen--;
        }

        if (!memcmp(p, keywords[j].ptr, klen) &&
            is_separator(*(p + klen))) {
          /* KeyWord */
          memset(row.hl + i, kw2 ? HL.KEYWORD2 : HL.KEYWORD1, klen);
          p += klen;
          i += klen;
          break;
        }
      }

      if (j < keywords.length) {
        prev_sep = false;
        continue;
      }
    }

    /* Not special chars */
    prev_sep = is_separator(*p);
    p++;
    i++;
  }

  /* Propagate syntax change to the next row if the open comment state changed.
   * This my recursively affect all the following rows in the file.
   */
  bool oc = editorRowHasOpenComment(row);

  if (row.hl_oc != oc && (row.idx + 1) < E.numrows) {
    editorUpdateSyntax(&E.row[(row.idx + 1)]);
  }

  row.hl_oc = oc;
}

/* Maps syntax highlight token types to terminal colors.  */
int editorSyntaxToColor(int hl) {
  with (HL) switch (hl) {
    case COMMENT:
    case MLCOMMENT: return 36; /* cyan */
    case KEYWORD1:  return 33; /* yellow */
    case KEYWORD2:  return 32; /* green */
    case STRING:    return 35; /* magenta */
    case NUMBER:    return 31; /* red */
    case MATCH:     return 34; /* blue */
    default:        return 37; /* white */
  }
}

/* Select the syntax highlight scheme depending on the filename, setting it in the global state E.syntax. */
void editorSelectSyntaxHighlight(string filename) {
  for (size_t idx; idx < HLDB_ENTRIES; idx++) {
    EditorSyntax* hl = &HLDB[idx];

    foreach (patn; hl.filematch) {
      if ((patn.length == filename.length && patn == filename) ||
          (filename.extension == patn)) {
        E.syntax = hl;
        return;
      }
    }
  }
}

/* Editor rows implementation */

/* Update the rendered version and the syntax highlight of a row. */
void editorUpdateRow(Erow* row) {
  ulong tabs,
        noprint,
        idx;

  /* Create a version of the row we can directly print on the screen,
   * respecting tabs, substituting non printable characters with '?'.
   */
  GC.free(row.render);

  static if (!USE_SPACE_INSTADE_OF_TAB) {
    for (ulong j; j < row.size; j++) {
      if (row.chars[j] == KEY_ACTION.TAB) {
        tabs++;
      }
    }
  }

  row.render = malloc!(char*)(row.size + (tabs * 8) + (noprint * 9) + 1, GC.BlkAttr.NO_SCAN | GC.BlkAttr.APPENDABLE);

  for (ulong j; j < row.size; j++) {
    static if (USE_SPACE_INSTADE_OF_TAB) {
      row.render[idx++] = row.chars[j];
    } else {
      with (KEY_ACTION) if (row.chars[j] == TAB) {
        row.render[idx++] = ' ';

        while ((idx + 1) % 8 != 0) {
          row.render[idx++] = ' ';
        }
      } else {
        row.render[idx++] = row.chars[j];
      }
    }
  }

  row.rsize       = idx;
  row.render[idx] = '\0';

  /* Update the syntax highlighting attributes of the row. */
  editorUpdateSyntax(row);
}

/* Insert a row at the specified position, shifting the other rows on the bottom if required. */
void editorInsertRow(ulong at, string s, size_t length = 0) {
  if (s.length > 0 && length == 0) {
    length = s.length;
  }

  if (at > E.numrows) {
    return;
  }

  E.row = realloc(E.row, (Erow).sizeof * (E.numrows + 1));

  if (at != E.numrows) {
    memmove(E.row + at + 1, E.row + at, (E.row[0]).sizeof*(E.numrows-at));    

    for (ulong j = at+1; j <= E.numrows; j++) {
      E.row[j].idx++;
    }
  }

  E.row[at].size  = length;
  E.row[at].chars = malloc!(char*)(length + 1);
  memcpy(E.row[at].chars, s.toStringz, length + 1);
  E.row[at].hl     = null;
  E.row[at].hl_oc  = false;
  E.row[at].render = null;
  E.row[at].rsize  = 0;
  E.row[at].idx    = at;
  editorUpdateRow(&E.row[at]);
  E.numrows++;
  E.dirty = true;
}

/* Free row's heap allocated stuff. */
void editorFreeRow(Erow* row) {
  GC.free(row.render);
  GC.free(row.chars);
  GC.free(row.hl);
}

/* Remove the row at the specified position, shifting the remaining on the top */
void editorDelRow(ulong at) {
  Erow* row;

  if (at >= E.numrows) {
    return;
  }

  row = E.row + at;
  editorFreeRow(row);
  memmove(E.row + at, E.row + at + 1, (E.row[0]).sizeof *(E.numrows - at - 1));

  for (ulong j = at; j < E.numrows-1; j++) {
    E.row[j].idx++;
  }

  E.numrows--;
  E.dirty = true;
}

string editorRowsToString() {
  string buf;

  for (ulong j; j < E.numrows; j++) {
    foreach (s; E.row[j].chars[0..E.row[j].size]) {
      buf ~= s;
    }

    buf ~= "\n";
  }

  return buf;
}

/* Insert a character at the specified position in a row, moving the remaining
 * chars on the right if needed.
 */
void editorRowInsertChar(Erow* row, ulong at, int c) {
  if (at > row.size) {
    /* Pad the string with spaces if the insert location is outside the
     * current length by more than a single character.
     */
    ulong padlen = at-row.size;
    /* In the next line +2 means: new char and null term. */
    row.chars = realloc(row.chars, row.size + padlen + 2);
    row.chars[row.size..(row.size + padlen)] = ' ';
    row.chars[row.size + padlen + 1] = '\0';
    row.size += padlen+1;
  } else {
    /* If we are in the middle of the string just make space for 1 new
     * char plus the (already existing) null term.
     */
    row.chars = realloc(row.chars, row.size + 2);
    memmove(row.chars + at + 1, row.chars + at, row.size - at + 1);

    row.size++;
  }

  row.chars[at] = cast(char)c;
  editorUpdateRow(row);
  E.dirty = true;
}

/* Append the string 's' at the end of a row */
void editorRowAppendString(Erow* row, string s) {
    row.chars = realloc(row.chars, row.size + s.length + 1);
    memcpy(row.chars + row.size, s.toStringz, s.length);    

    row.size           += s.length;
    row.chars[row.size] = '\0';
    editorUpdateRow(row);
    E.dirty = true;
}

/* Delete the character at offset 'at' from the specified row. */
void editorRowDelChar(Erow* row, ulong at) {
  if (row.size <= at) {
    return;
  }

  memmove(row.chars + at, row.chars + at + 1, row.size - at);
  editorUpdateRow(row);
  row.size--;
  E.dirty = true;
}

/* Insert the specified char at the current prompt position. */
void editorInsertChar(int c) {
  ulong filerow = E.rowoff+E.cy;
  ulong filecol = E.coloff+E.cx;
  Erow* row = (filerow >= E.numrows) ? null : &E.row[filerow];

  /* If the row where the cursor is currently located does not exist in our
   * logical representaion of the file, add enough empty rows as needed.
   */
  if (!row) {
    while (E.numrows <= filerow) {
      editorInsertRow(E.numrows, "");
    }
  }

  row = &E.row[filerow];
  editorRowInsertChar(row, filecol, c);
  
  if (E.cx == E.screencols-1) {
    E.coloff++;
  } else {
    E.cx++;
  }

  E.dirty = true;
}

/* Inserting a newline is slightly complex as we have to handle inserting a
 * newline in the middle of a line, splitting the line as needed.
 */
void editorInsertNewline() {
  ulong filerow = E.rowoff+E.cy;
  ulong filecol = E.coloff+E.cx;
  Erow* row     = (filerow >= E.numrows) ? null : &E.row[filerow];

  if (!row) {
    if (filerow == E.numrows) {
      editorInsertRow(filerow, "");
      goto fixcursor;
    }

    return;
  }

  /* If the cursor is over the current line size, we want to conceptually
   * think it's just over the last character.
   */
  if (filecol >= row.size) {
    filecol = row.size;
  }

  if (filecol == 0) {
    editorInsertRow(filerow, "");
  } else {
    /* We are in the middle of a line. Split it between two rows. */
    editorInsertRow(filerow+1, (row.chars + filecol).to!string, row.size - filecol);

    row                = &E.row[filerow];
    row.chars[filecol] = '\0';
    row.size           = filecol;
    editorUpdateRow(row);
  }

fixcursor:
  if (E.cy == E.screenrows - 1) {
    E.rowoff++;
  } else {
    E.cy++;
  }

  E.cx = 0;
  E.coloff = 0;
}

/* Delete the char at the current prompt position. */
void editorDelChar() {
  ulong filerow = E.rowoff + E.cy;
  ulong filecol = E.coloff + E.cx;
  Erow* row     = (filerow >= E.numrows) ? null : &E.row[filerow];

  if (!row || (filecol == 0 && filerow == 0)) {
    return;
  }

  if (filecol == 0) {
    /* Handle the case of column 0, we need to move the current line
     * on the right of the previous one.
     */
    filecol = E.row[filerow - 1].size;
    editorRowAppendString(&E.row[filerow - 1], row.chars[0..row.size].to!string);
    editorDelRow(filerow);
    row = null;

    if (E.cy == 0) {
      E.rowoff--;
    } else {
      E.cy--;
    }

    E.cx = filecol;
    
    if (E.cx >= E.screencols) {
      ulong shift = (E.screencols - E.cx) + 1;
      E.cx       -= shift;
      E.coloff   += shift;
    }
  } else {
    editorRowDelChar(row, filecol - 1);

    if (E.cx == 0 && E.coloff) {
      E.coloff--;
    } else {
      E.cx--;
    }
  }
  if (row) editorUpdateRow(row);
  E.dirty = true;
}

/* Load the specified program in the editor memory and returns 0 on success
 * or 1 on error.
 */
int editorOpen(string filename) {
  E.dirty    = false;
  E.filename = filename.dup;

  if (!exists(filename)) {
    return 1;
  }

  auto file = File(filename, "r");

  foreach (line; file.byLine) {
    editorInsertRow(E.numrows, line.to!string);
  }

  E.dirty = false;

  return 0;
}

/* Save the current file on disk. Return 0 on success, 1 on error. */
int editorSave() {
  int    len;
  string buf  = editorRowsToString();
  auto   file = File(E.filename, "w");

  file.write(buf);

  editorSetStatusMessage("%d bytes written on disk", buf.length);

  E.dirty = false;

  return 0;
}

/* Terminal Update */

/* This function writes the whole screen using VT100 escape characters
 * starting from the logical state of the editor in the global state 'E'.
 */
void editorRefreshScreen() {
  ulong  y;
  Erow*  r;
  string str;

  str ~= "\x1b[?25l"; /* Hide cursor. */
  str ~= "\x1b[H";    /* Go home. */

  for (; y < E.screenrows; y++) {
    ulong filerow = E.rowoff + y;

    if (filerow >= E.numrows) {
      if (E.numrows == 0 && y == E.screenrows / 3) {
        Appender!string welcome = appender!string;
        
        formattedWrite(welcome, "Dilo editor -- verison %s\x1b[0K\r\n", DILO_VERSION);

        ulong padding = (E.screencols - welcome.data.length) / 2;

        if (padding) {
          str ~= "~";
          padding--;
        }

        while (padding--) {
          str ~= " ";
        }

        str ~= welcome.data;
      } else {
        str ~= "~\x1b[0K\r\n";
      }
      continue;
    }

    r = &E.row[filerow];

    ulong len           = r.rsize - E.coloff;
    int   current_color = -1;

    if (len > 0) {
      if (len > E.screencols) {
        len = E.screencols;
      }

      char*  c  = r.render + E.coloff;
      ubyte* hl = r.hl + E.coloff;

      for (ulong j; j < len; j++) {
        if (hl[j] == HL.NONPRINT) {
          char sym;
          str ~= "\x1b[7m";

          if (c[j] <= 26) {
            sym = cast(char)('@'+c[j]);
          } else {
            sym = '?';
          }

          str ~= sym;
          str ~= "\x1b[0m";
        } else if (hl[j] == HL.NORMAL) {
          if (current_color != -1) {
            str ~= "\x1b[39m";
            current_color = -1;
          }

          str ~= c[j];
        } else {
          int color = editorSyntaxToColor(hl[j]);

          if (color != current_color) {
            auto buf = appender!string;
            formattedWrite(buf, "\x1b[%dm", color);
            current_color = color;
            str ~= buf.data;
          }

          str ~= c[j];
        }
      }
    }

    str ~= "\x1b[39m";
    str ~= "\x1b[0K";
    str ~= "\r\n";
  }

  /* Create a two rows status. First row: */
  str ~= "\x1b[0K";
  str ~= "\x1b[7m";

  auto status  = appender!string,
       rstatus = appender!string;

  string filetype = "plain text";

  if (E.syntax !is null) {
    filetype = E.syntax.syntaxName;
  }
  
  formattedWrite(status, "%.20s [filetype: %s]- %d lines %s", E.filename, filetype, E.numrows, E.dirty ? "(modified)" : "");
  formattedWrite(rstatus, "%d/%d", E.rowoff + E.cy + 1, E.numrows);

  ulong len  = status.data.length,
        rlen = rstatus.data.length;

  if (len > E.screencols) {
    len = E.screencols;
  }

  str ~= status.data;

  while(len < E.screencols) {
    if (E.screencols - len == rlen) {
      str ~= rstatus.data;
      break;
    } else {
      str ~= " ";
      len++;
    }
  }
  
  str ~= "\x1b[0m\r\n";

  /* Second row depends on E.statusmsg and the status message update time. */
  str ~= "\x1b[0K";
  ulong msglen = E.statusmsg.data.length;
  if (msglen && time(null) - E.statusmsg_time < 5) {
    str ~= E.statusmsg.data;
  }

  /* Put cursor at its current position. Note that the horizontal position
   * at which the cursor is displayed may be different compared to 'E.cx'
   * because of TABs.
   */
  ulong j;
  ulong cx      = 1;
  ulong filerow = E.rowoff + E.cy;
  Erow* row = (filerow >= E.numrows) ? null : &E.row[filerow];

  if (row) {
    for (j = E.coloff; j < (E.cx+E.coloff); j++) {
      with (KEY_ACTION) if (j < row.size && row.chars[j] == TAB) {
        cx += 7 - ((cx) % 8);
      }
      cx++;
    }
  }

  auto buf = appender!string;

  formattedWrite(buf, "\x1b[%d;%dH", E.cy + 1, cx);
  str ~= buf.data;
  str ~= "\x1b[?25h"; /* Show cursor. */
  core.sys.posix.unistd.write(STDOUT_FILENO, str.toStringz, str.length);
}

/* Set an editor status message for the second line of the status, at the
 * end of the screen.
 */
void editorSetStatusMessage(FMT, A...)(FMT fmt, A a) if (is(FMT == string)) {
  E.statusmsg = appender!string;  
  formattedWrite(E.statusmsg, fmt, a);
  E.statusmsg_time = time(null);
}

/* Find mode */
enum DILO_QUERY_LEN = 256;

void editorFind(int fd) {
  char[DILO_QUERY_LEN+1] query = 0;
  ulong qlen          = 0;
  long  last_match    = -1;  /* Last line where a match was found. -1 for none. */
  long  find_next     = 0;   /* if 1 search next, if -1 search prev. */
  long  saved_hl_line = -1;  /* No saved HL */
  char* saved_hl      = null;

  enum FIND_RESTORE_HL = q{
    do {
      if (saved_hl) { 
        memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);        
        saved_hl = null; 
      } 
    } while (0);
  };

  /* Save the cursor position in order to restore it later. */
  ulong saved_cx     = E.cx,
        saved_cy     = E.cy;
  ulong saved_coloff = E.coloff,
        saved_rowoff = E.rowoff;

  with (KEY_ACTION) while (1) {
    editorSetStatusMessage("Search: %s (Use ESC/Arrows/Enter)", query);
    editorRefreshScreen();

    int c = editorReadKey(fd);

    if (c == DEL_KEY || c == CTRL_H || c == BACKSPACE) {
      if (qlen != 0) {
        query[--qlen] = '\0';
      }

      last_match = -1;
    } else if (c == ESC || c == ENTER) {
      if (c == ESC) {
        E.cx     = saved_cx; E.cy = saved_cy;
        E.coloff = saved_coloff; E.rowoff = saved_rowoff;
      }

      mixin(FIND_RESTORE_HL);
      editorSetStatusMessage("");

      return;
    } else if (c == ARROW_RIGHT || c == ARROW_DOWN) {
      find_next = 1;
    } else if (c == ARROW_LEFT || c == ARROW_UP) {
      find_next = -1;
    } else if (isprint(c)) {
      if (qlen < DILO_QUERY_LEN) {
        query[qlen++] = cast(char)c;
        query[qlen]   = '\0';
        last_match    = -1;
      }
    }

    /* Search occurrence. */
    if (last_match == -1) {
      find_next = 1;
    }

    if (find_next) {
      char* match;
      long  match_offset;
      long  current = last_match;

      for (ulong i; i < E.numrows; i++) {
        current += find_next;

        if (current == -1) {
          current = E.numrows - 1;
        } else if (current == E.numrows) {
          current = 0;
        }

        // Need replacing with D function
        import core.stdc.string;
        match = strstr(E.row[current].render, query.ptr);

        if (match) {
          match_offset = match-E.row[current].render;
          break;
        }
      }

      find_next = 0;

      /* Highlight */
      mixin(FIND_RESTORE_HL);

      if (match !is null) {
        Erow* row  = &E.row[current];
        last_match = current;

        if (row.hl) {
          saved_hl_line = current;
          saved_hl      = malloc!(char*)(row.rsize);
          memcpy(saved_hl, row.hl, row.rsize);
          memset(row.hl + match_offset, HL.MATCH, qlen);
        }

        E.cy     = 0;
        E.cx     = match_offset;
        E.rowoff = current;
        E.coloff = 0;

        /* Scroll horizontally as needed. */
        if (E.cx > E.screencols) {
          ulong diff = E.cx - E.screencols;
          E.cx     -= diff;
          E.coloff += diff;
        }
      }
    }
  }
}

/* Editor events handling */
/* Handle cursor position change because arrow keys were pressed. */
void editorMoveCursor(int key) {
  ulong filerow = E.rowoff + E.cy;
  ulong filecol = E.coloff + E.cx;
  Erow* row     = (filerow >= E.numrows) ? null : &E.row[filerow];
  ulong rowlen;

  with (KEY_ACTION) switch(key) {
    case ARROW_LEFT:
      if (E.cx == 0) {
        if (E.coloff) {
          E.coloff--;
        } else {
          if (filerow > 0) {
            E.cy--;
            E.cx = E.row[filerow - 1].size;

            if (E.cx > E.screencols - 1) {
              E.coloff = E.cx - E.screencols + 1;
              E.cx = E.screencols - 1;
            }
          }
        }
      } else {
        E.cx -= 1;
      }
      break;
    case ARROW_RIGHT:
      if (row && filecol < row.size) {
        if (E.cx == E.screencols - 1) {
          E.coloff++;
        } else {
          E.cx += 1;
        }
      } else if (row && filecol == row.size) {
        E.cx     = 0;
        E.coloff = 0;

        if (E.cy == E.screenrows - 1) {
          E.rowoff++;
        } else {
          E.cy += 1;
        }
      }
      break;
    case ARROW_UP:
      if (E.cy == 0) {
        if (E.rowoff) {
          E.rowoff--;
        }
      } else {
        E.cy -= 1;
      }
      break;
    case ARROW_DOWN:
      if (filerow < E.numrows) {
        if (E.cy == E.screenrows - 1) {
          E.rowoff++;
        } else {
          E.cy += 1;
        }
      }
      break;
    default: break;
  }

  /* Fix cx if the current line has not enough chars. */
  filerow = E.rowoff+E.cy;
  filecol = E.coloff+E.cx;
  row     = (filerow >= E.numrows) ? null : &E.row[filerow];
  rowlen  = row ? row.size : 0;

  if (filecol > rowlen) {
    E.cx -= filecol - rowlen;

    if (E.cx < 0) {
      E.coloff += E.cx;
      E.cx      = 0;
    }
  }
}

/* Process events arriving from the standard input, which is, the user
 * is typing stuff on the terminal.
 */
enum DILO_QUIT_TIMES = 3;

void editorProcessKeypress(int fd) {
  /* When the file is modified, requires Ctrl-q to be pressed N times
   * before actually quitting.
   */
  static int quit_times = DILO_QUIT_TIMES;

  int c = editorReadKey(fd);

  with (KEY_ACTION) switch (c) {
    case ENTER:         /* Enter */
      editorInsertNewline();
      break;
    case CTRL_C:        /* Ctrl-c */
      /* We ignore ctrl-c, it can't be so simple to lose the changes
       * to the edited file. */
      break;
    case CTRL_Q:        /* Ctrl-q */
      /* Quit if the file was already saved. */
      if (E.dirty && quit_times) {
        editorSetStatusMessage("WARNING!!! File has unsaved changes. "
            "Press Ctrl-Q %s more times to quit.", quit_times.to!string);
        quit_times--;
        return;
      }

      exit(0);

      break;
    case CTRL_S:        /* Ctrl-s */
      editorSave();
      break;
    case CTRL_F:
      editorFind(fd);
      break;
    case BACKSPACE:     /* Backspace */
    case CTRL_H:        /* Ctrl-h */
    case DEL_KEY:
      editorDelChar();
      break;
    case PAGE_UP:
    case PAGE_DOWN:
      if (c == PAGE_UP && E.cy != 0) {
        E.cy = 0;
      } else if (c == PAGE_DOWN && E.cy != E.screenrows - 1) {
        E.cy = E.screenrows - 1;
      }

      ulong times = E.screenrows;
      
      while(times--) {
        editorMoveCursor(c == PAGE_UP ? ARROW_UP: ARROW_DOWN);
      }

      break;
    case ARROW_UP:
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
      editorMoveCursor(c);
      break;
    case CTRL_L: /* ctrl+l, clear screen */
      /* Just refresht the line as side effect. */
      break;
    case ESC:
      /* Nothing to do for ESC in this mode. */
      break;
    case TAB:
      static if (USE_SPACE_INSTADE_OF_TAB) {
        for (int j = 0; j < TAB_SPACE_SIZE; j++) {
          editorInsertChar(' ');
        }
      }

      break;
    default:
      editorInsertChar(c);
      break;
  }

  quit_times = DILO_QUIT_TIMES; /* Reset it to the original value. */
}

bool editorFileWasModified() {
  return E.dirty;
}

void initEditor() {
  E.cx       = 0;
  E.cy       = 0;
  E.rowoff   = 0;
  E.coloff   = 0;
  E.numrows  = 0;
  E.row      = null;
  E.dirty    = 0;
  E.filename = null;
  E.syntax   = null;

  if (getWindowSize(STDIN_FILENO, STDOUT_FILENO, &E.screenrows, &E.screencols) == -1) {
    perror("Unable to query the screen for size (columns / rows)");
    exit(1);
  }

  E.screenrows -= 2; /* Get room for status bar. */
}

void main(string[] args) {
  if (args.length != 2) {
    stderr.writeln("Usage: dilo <filename>");
    exit(1);
  }

  initEditor();
  editorSelectSyntaxHighlight(args[1]);
  editorOpen(args[1]);
  enableRawMode(STDIN_FILENO);
  editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

  while (1) {
    editorRefreshScreen();
    editorProcessKeypress(STDIN_FILENO);
  }
}
