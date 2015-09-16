/*

Copyright (c) 2012, Lambda Foundry, Inc., except where noted

Incorporates components of WarrenWeckesser/textreader, licensed under 3-clause
BSD

   Low-level ascii-file processing from pandas. Combines some elements from
   Python's built-in csv module and Warren Weckesser's textreader project on
   GitHub. See Python Software Foundation License and BSD licenses for these.

   Heavily adapted for tarray/Tcl
*/

#ifndef _TACSV_H
#define _TACSV_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#if defined(_MSC_VER)
#include "ms_stdint.h"
#else
#include <stdint.h>
#endif

#include "tcl.h"
#include "khash.h"

#define CHUNKSIZE 1024*256
#define KB 1024
#define MB 1024 * KB
#define STREAM_INIT_SIZE 32

#define REACHED_EOF 1
#define CALLING_READ_FAILED 2

#ifndef P_INLINE
  #if defined(__GNUC__)
    #define P_INLINE __inline__
  #elif defined(_MSC_VER)
    #define P_INLINE
  #elif defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
    #define P_INLINE inline
  #else
    #define P_INLINE
  #endif
#endif


/* #define VERBOSE */

#if defined(VERBOSE)
#define TRACE(X) printf X;
#else
#define TRACE(X)
#endif


#define PARSER_OUT_OF_MEMORY -1


/*
 *  XXX Might want to couple count_rows() with read_rows() to avoid duplication
 *      of some file I/O.
 */

/*
 *  WORD_BUFFER_SIZE determines the maximum amount of non-delimiter
 *  text in a row.
 */
#define WORD_BUFFER_SIZE 4000


typedef enum {
    START_RECORD,
    START_FIELD,
    ESCAPED_CHAR,
    IN_FIELD,
    IN_QUOTED_FIELD,
    ESCAPE_IN_QUOTED_FIELD,
    QUOTE_IN_QUOTED_FIELD,
    EAT_CRNL,
    EAT_CRNL_NOP,
    EAT_WHITESPACE,
    EAT_COMMENT,
    EAT_LINE_COMMENT,
    WHITESPACE_LINE,
    SKIP_LINE,
    FINISHED
} ParserState;

typedef enum {
    QUOTE_MINIMAL, QUOTE_ALL, QUOTE_NONNUMERIC, QUOTE_NONE
} QuoteStyle;


typedef struct parser_t {
    Tcl_Channel chan;

    int chunksize;  // Number of bytes to prepare for each chunk
    Tcl_Obj *dataObj; // Tcl_Obj where data is read from channel
    char *data;     // Points into dataObj (data to be processed)
    int datalen;    // amount of data available
    int datapos;

    // Tcl_Obj containing the read rows
    Tcl_Obj *rowsObj; // List of built rows
    Tcl_Obj *rowObj;  // The row being built
    Tcl_Obj *fieldObj; // The field being built

    int lines;            // Number of (good) lines observed
    int file_lines;       // Number of file lines observed (including bad or skipped)

    // Tokenizing stuff
    ParserState state;
    int doublequote;            /* is " represented by ""? */
    char delimiter;             /* field separator */
    int delim_whitespace;       /* delimit by consuming space/tabs instead */
    char quotechar;             /* quote character */
    char escapechar;            /* escape character */
    char lineterminator;
    int skipinitialspace;       /* ignore spaces following delimiter? */
    int quoting;                /* style of quoting to write */

    // krufty, hmm =/
    int numeric_field;

    char commentchar;
    int allow_embedded_newline;
    int strict;                 /* raise exception on bad CSV */

    int expected_fields;
    int error_bad_lines;
    int warn_bad_lines;

    int header; // Boolean: 1: has header, 0: no header
    int header_start; // header row start
    int header_end;   // header row end

    void *skipset;
    int64_t skip_first_N_rows;
    int skip_footer;

    // error handling
    char *warn_msg;
    char *error_msg;

    int skip_empty_lines;
} parser_t;

parser_t* parser_new(void);

int parser_init(parser_t *self);

int parser_add_skiprow(parser_t *self, int64_t row);

int parser_set_skipfirstnrows(parser_t *self, int64_t nrows);

void parser_free(parser_t *self);

void parser_set_default_options(parser_t *self);

void debug_print_parser(parser_t *self);

int tokenize_nrows(parser_t *self, size_t nrows);

int tokenize_all_rows(parser_t *self);

#endif // _PARSER_COMMON_H_
