/*

  CSV parsing - Heavily adapted for tarray/Tcl 

  From:
  Copyright (c) 2012, Lambda Foundry, Inc., except where noted

  Incorporates components of WarrenWeckesser/textreader, licensed under 3-clause
  BSD
   Low-level ascii-file processing from pandas. Combines some elements from
   Python's built-in csv module and Warren Weckesser's textreader project on
   GitHub. See Python Software Foundation License and BSD licenses for these.

*/

#include "tarray.h"
#include "tacsv.h"

KHASH_MAP_INIT_INT64(int64, size_t)

static void free_if_not_null(void **ptr) {
    if (*ptr != NULL) {
        free(*ptr);
        *ptr = NULL;
    }
}

void parser_set_default_options(parser_t *self) {

    // For tokenization
    self->state = START_RECORD;

    self->delimiter = ','; // XXX
    self->delim_whitespace = 0;

    self->doublequote = 0;
    self->quotechar = '"';
    self->escapechar = 0;

    self->lineterminator = '\0'; /* NUL->standard logic */

    self->skipinitialspace = 0;
    self->quoting = QUOTE_MINIMAL;
    self->allow_embedded_newline = 1;
    self->strict = 0;

    self->expected_fields = -1;
    self->error_bad_lines = 0;
    self->warn_bad_lines = 0;

    self->commentchar = '#';

    self->skipset = NULL;
    self-> skip_first_N_rows = -1;
    self->skip_footer = 0;
}

parser_t* parser_new() {
    return (parser_t*) calloc(1, sizeof(parser_t));
}

int parser_cleanup(parser_t *self) {
    int    status = 0;

    // XXX where to put this
    free_if_not_null((void *) &self->error_msg);
    free_if_not_null((void *) &self->warn_msg);

    if (self->skipset != NULL) {
        kh_destroy_int64((kh_int64_t*) self->skipset);
        self->skipset = NULL;
    }

    if (self->dataObj) {
        Tcl_DecrRefCount(self->dataObj);
        self->dataObj = NULL;
    }
    
    if (self->rowsObj) {
        Tcl_DecrRefCount(self->rowsObj);
        self->rowsObj = NULL;
    }

    if (self->rowObj) {
        Tcl_DecrRefCount(self->rowObj);
        self->rowObj = NULL;
    }
    
    if (self->fieldObj) {
        Tcl_DecrRefCount(self->fieldObj);
        self->fieldObj = NULL;
    }
    
    return status;
}

int parser_init(parser_t *self) {
    self->error_msg = NULL;
    self->warn_msg = NULL;

    self->lines = 0;
    self->file_lines = 0;

    /* read bytes buffered */
    self->dataObj = Tcl_NewObj();
    Tcl_IncrRefCount(self->dataObj);
    self->data = Tcl_GetStringFromObj(self->dataObj, &self->datalen);
    self->datapos = 0;

    /* Where we collect the rows */
    self->rowsObj = Tcl_NewListObj(1000, NULL); /* TBD - guess 1000 rows */
    Tcl_IncrRefCount(self->rowsObj);
    self->rowObj = Tcl_NewListObj(10, NULL); /* TBD - Guess 10 fields */
    Tcl_IncrRefCount(self->rowObj);
    self->fieldObj = Tcl_NewObj();
    Tcl_IncrRefCount(self->fieldObj);
    
    self->state = START_RECORD;

    self->commentchar = '\0';

    return 0;
}


void parser_free(parser_t *self) {
    // opposite of parser_init
    parser_cleanup(self);
    free(self);
}

static int P_INLINE end_field(parser_t *self) {
    Tcl_ListObjAppendElement(NULL, self->rowObj, self->fieldObj);
    self->fieldObj = Tcl_NewObj();
    Tcl_IncrRefCount(self->fieldObj);

    return 0;
}

static void append_warning(parser_t *self, const char *msg) {
    int ex_length;
    int length = strlen(msg);
    void *newptr;

    if (self->warn_msg == NULL) {
        self->warn_msg = (char*) malloc(length + 1);
        strcpy(self->warn_msg, msg);
    } else {
        ex_length = strlen(self->warn_msg);
        newptr = realloc(self->warn_msg, ex_length + length + 1);
        if (newptr != NULL) {
            self->warn_msg = (char*) newptr;
            strcpy(self->warn_msg + ex_length, msg);
        }
    }
}

static int end_line(parser_t *self) {
    int fields;
    int ex_fields = self->expected_fields;

#ifndef TBD
    /* TBD - don't deal with special cases */
    fields = 0;
    Tcl_ListObjLength(NULL, self->rowObj,  &fields);
    Tcl_ListObjAppendElement(NULL, self->rowsObj, self->rowObj);
    self->rowObj = Tcl_NewListObj(fields, NULL);
    Tcl_IncrRefCount(self->rowObj);

    TRACE(("end_line: Line end, nfields: %d\n", fields));

    self->file_lines++;
    self->lines++;
#else // TBD
    if (self->lines > 0) {
        if (self->expected_fields >= 0) {
            ex_fields = self->expected_fields;
        } else {
            ex_fields = self->line_fields[self->lines - 1];
        }
    }

    if (self->state == SKIP_LINE) {
        TRACE(("end_line: Skipping row %d\n", self->file_lines));
        // increment file line count
        self->file_lines++;

        // skip the tokens from this bad line
        self->line_start[self->lines] += fields;

        // reset field count
        self->line_fields[self->lines] = 0;
        return 0;
    }
    /* printf("Line: %d, Fields: %d, Ex-fields: %d\n", self->lines, fields, ex_fields); */

    if (!(self->lines <= self->header_end + 1)
        && (self->expected_fields < 0 && fields > ex_fields)) {
        // increment file line count
        self->file_lines++;

        // skip the tokens from this bad line
        self->line_start[self->lines] += fields;

        // reset field count
        self->line_fields[self->lines] = 0;

        // file_lines is now the _actual_ file line number (starting at 1)

        if (self->error_bad_lines) {
            self->error_msg = (char*) malloc(100);
            sprintf(self->error_msg, "Expected %d fields in line %d, saw %d\n",
                    ex_fields, self->file_lines, fields);

            TRACE(("Error at line %d, %d fields\n", self->file_lines, fields));

            return -1;
        } else {
            // simply skip bad lines
            if (self->warn_bad_lines) {
                // pass up error message
                msg = (char*) malloc(100);
                sprintf(msg, "Skipping line %d: expected %d fields, saw %d\n",
                        self->file_lines, ex_fields, fields);
                append_warning(self, msg);
                free(msg);
            }
        }
    }
    else {
        /* missing trailing delimiters */
        if ((self->lines >= self->header_end + 1) && fields < ex_fields) {

            /* Might overrun the buffer when closing fields */
            if (make_stream_space(self, ex_fields - fields) < 0) {
                self->error_msg = "out of memory";
                return -1;
            }

            while (fields < ex_fields){
                end_field(self);
                /* printf("Prior word: %s\n", self->words[self->words_len - 2]); */
                fields++;
            }
        }

        // increment both line counts
        self->file_lines++;

        self->lines++;

        /* coliter_t it; */
        /* coliter_setup(&it, self, 5, self->lines - 1); */
        /* printf("word at column 5: %s\n", COLITER_NEXT(it)); */

        // good line, set new start point
        if (self->lines >= self->lines_cap) {
            TRACE(("end_line: ERROR!!! self->lines(%zu) >= self->lines_cap(%zu)\n", self->lines, self->lines_cap))  \
            self->error_msg = (char*) malloc(100);      \
            sprintf(self->error_msg, "Buffer overflow caught - possible malformed input file.\n"); \
            return PARSER_OUT_OF_MEMORY;                \
        }
        self->line_start[self->lines] = (self->line_start[self->lines - 1] +
                                         fields);

        TRACE(("end_line: new line start: %d\n", self->line_start[self->lines]));

        // new line start with 0 fields
        self->line_fields[self->lines] = 0;
    }
#endif // TBD
    
    TRACE(("end_line: Finished line, at %d\n", self->lines));

    return 0;
}

int parser_add_skiprow(parser_t *self, int64_t row) {
    khiter_t k;
    kh_int64_t *set;
    int ret = 0;

    if (self->skipset == NULL) {
        self->skipset = (void*) kh_init_int64();
    }

    set = (kh_int64_t*) self->skipset;

    k = kh_put_int64(set, row, &ret);
    set->keys[k] = row;

    return 0;
}

int parser_set_skipfirstnrows(parser_t *self, int64_t nrows) {
    // self->file_lines is zero based so subtract 1 from nrows
    if (nrows > 0) {
        self->skip_first_N_rows = nrows - 1;
    }

    return 0;
}

static int parser_buffer_bytes(parser_t *self, size_t nbytes) {
    int status, chars_read;

    status = 0;
    self->datapos = 0;
    if (self->dataObj == NULL)
        self->dataObj = Tcl_NewObj();
    
    chars_read = Tcl_ReadChars(self->chan, self->dataObj, nbytes, 0);
    if (chars_read > 0) {
        self->data = Tcl_GetStringFromObj(self->dataObj, &self->datalen);
        return 0; /* Success */
    } else if (chars_read == 0) {
        /* Currently treat as EOF as we do not handle non-blocking chans */
        self->datalen = 0;
        return REACHED_EOF;
    } else {
        self->error_msg = (char*) malloc(200);
        sprintf(self->error_msg, "Calling read(nbytes) on source failed (Error %d).", Tcl_GetErrno());
        return -1;
    }
}


/*
  Tokenization macros and state machine code
*/

//    printf("pushing %c\n", c);

#define PUSH_CHAR(c)                            \
    do {                                        \
        TRACE(("PUSH_CHAR: Pushing %c\n", c))   \
        Tcl_AppendToObj(self->fieldObj, &c, 1); \
    } while (0)

// This is a little bit of a hack but works for now
#define END_FIELD()                             \
    do {                                        \
        if (end_field(self) < 0) {              \
            goto parsingerror;                  \
        }                                       \
    } while (0)

#define END_LINE_STATE(STATE)                                           \
    do {                                                                \
        if (end_line(self) < 0) {                                       \
            goto parsingerror;                                          \
        }                                                               \
        self->state = STATE;                                            \
        if (line_limit > 0 && self->lines == start_lines + line_limit) { \
            goto linelimit;                                             \
                                                                        \
        }                                                               \
    } while (0)

#define END_LINE_AND_FIELD_STATE(STATE)                                 \
    do {                                                                \
        if (end_line(self) < 0) {                                       \
            goto parsingerror;                                          \
        }                                                               \
        if (end_field(self) < 0) {                                      \
            goto parsingerror;                                          \
        }                                                               \
        self->state = STATE;                                            \
        if (line_limit > 0 && self->lines == start_lines + line_limit) { \
            goto linelimit;                                             \
                                                                        \
        }                                                               \
    } while (0)

#define END_LINE() END_LINE_STATE(START_RECORD)

#define IS_WHITESPACE(c) ((c == ' ' || c == '\t'))

typedef int (*parser_op)(parser_t *self, size_t line_limit);

#define _TOKEN_CLEANUP()                                                \
    do { \
        self->datapos = i;                                              \
        TRACE(("_TOKEN_CLEANUP: datapos: %d, datalen: %d\n", self->datapos, self->datalen)); \
    } while (0)

int skip_this_line(parser_t *self, int64_t rownum) {
    if (self->skipset != NULL) {
        return ( kh_get_int64((kh_int64_t*) self->skipset, self->file_lines) !=
                 ((kh_int64_t*)self->skipset)->n_buckets );
    }
    else {
        return ( rownum <= self->skip_first_N_rows );
    }
}

int tokenize_delimited(parser_t *self, size_t line_limit)
{
    int i, start_lines;
    char c;
    char *buf = self->data + self->datapos;

    start_lines = self->lines;

    TRACE(("%s\n", buf));

    for (i = self->datapos; i < self->datalen; ++i)
    {
        // Next character in file
        c = *buf++;

        TRACE(("tokenize_delimited - Iter: %d Char: 0x%x Line %d, state %d\n",
               i, c, self->file_lines + 1, self->state));

        switch(self->state) {

        case SKIP_LINE:
            TRACE(("tokenize_delimited SKIP_LINE 0x%x, state %d\n", c, self->state));
            if (c == '\n') {
                END_LINE();
            } else if (c == '\r') {
                self->file_lines++;
                self->state = EAT_CRNL_NOP;
            }
            break;

        case START_RECORD:
            // start of record
            if (skip_this_line(self, self->file_lines)) {
                self->state = SKIP_LINE;
                if (c == '\n') {
                    END_LINE();
                }
                break;
            }
            else if (c == '\n') {
                // \n\r possible?
                if (self->skip_empty_lines)
                {
                    self->file_lines++;
                }
                else
                {
                    END_LINE();
                }
                break;
            }
            else if (c == '\r') {
                if (self->skip_empty_lines)
                {
                    self->file_lines++;
                    self->state = EAT_CRNL_NOP;
                }
                else
                    self->state = EAT_CRNL;
                break;
            }
            else if (c == self->commentchar) {
                self->state = EAT_LINE_COMMENT;
                break;
            }
            else if (IS_WHITESPACE(c) && c != self->delimiter && self->skip_empty_lines) {
                self->state = WHITESPACE_LINE;
                break;
            }

            /* normal character - handle as START_FIELD */
            self->state = START_FIELD;
            /* fallthru */

        case START_FIELD:
            /* expecting field */
            if (c == '\n') {
                END_FIELD();
                END_LINE();
            } else if (c == '\r') {
                END_FIELD();
                self->state = EAT_CRNL;
            }
            else if (c == self->quotechar &&
                     self->quoting != QUOTE_NONE) {
                /* start quoted field */
                self->state = IN_QUOTED_FIELD;
            }
            else if (c == self->escapechar) {
                /* possible escaped character */
                self->state = ESCAPED_CHAR;
            }
            else if (c == ' ' && self->skipinitialspace)
                /* ignore space at start of field */
                ;
            else if (c == self->delimiter) {
                /* save empty field */
                END_FIELD();
            }
            else if (c == self->commentchar) {
                END_FIELD();
                self->state = EAT_COMMENT;
            }
            else {
                /* begin new unquoted field */
//                if (self->quoting == QUOTE_NONNUMERIC)
//                    self->numeric_field = 1;

                // TRACE(("pushing %c", c));
                PUSH_CHAR(c);
                self->state = IN_FIELD;
            }
            break;

        case WHITESPACE_LINE: // check if line is whitespace-only
            if (c == '\n') {
                self->file_lines++;
                self->state = START_RECORD; // ignore empty line
            }
            else if (c == '\r') {
                self->file_lines++;
                self->state = EAT_CRNL_NOP;
            }
            else if (IS_WHITESPACE(c) && c != self->delimiter)
                ;
            else { // backtrack
                /* We have to use i + 1 because buf has been incremented but not i */
                do {
                    --buf;
                    --i;
                } while (i + 1 > self->datapos && *buf != '\n');

                if (*buf == '\n') // reached a newline rather than the beginning
                {
                    ++buf; // move pointer to first char after newline
                    ++i;
                }
                self->state = START_FIELD;
            }
            break;

        case ESCAPED_CHAR:
            /* if (c == '\0') */
            /*  c = '\n'; */

            PUSH_CHAR(c);
            self->state = IN_FIELD;
            break;

        case EAT_LINE_COMMENT:
            if (c == '\n') {
                self->file_lines++;
                self->state = START_RECORD;
            } else if (c == '\r') {
                self->file_lines++;
                self->state = EAT_CRNL_NOP;
            }
            break;

        case IN_FIELD:
            /* in unquoted field */
            if (c == '\n') {
                END_FIELD();
                END_LINE();
                /* self->state = START_RECORD; */
            } else if (c == '\r') {
                END_FIELD();
                self->state = EAT_CRNL;
            }
            else if (c == self->escapechar) {
                /* possible escaped character */
                self->state = ESCAPED_CHAR;
            }
            else if (c == self->delimiter) {
                // End of field. End of line not reached yet
                END_FIELD();
                self->state = START_FIELD;
            }
            else if (c == self->commentchar) {
                END_FIELD();
                self->state = EAT_COMMENT;
            }
            else {
                /* normal character - save in field */
                PUSH_CHAR(c);
            }
            break;

        case IN_QUOTED_FIELD:
            /* in quoted field */
            if (c == self->escapechar) {
                /* Possible escape character */
                self->state = ESCAPE_IN_QUOTED_FIELD;
            }
            else if (c == self->quotechar &&
                     self->quoting != QUOTE_NONE) {
                if (self->doublequote) {
                    /* doublequote; " represented by "" */
                    self->state = QUOTE_IN_QUOTED_FIELD;
                }
                else {
                    /* end of quote part of field */
                    self->state = IN_FIELD;
                }
            }
            else {
                /* normal character - save in field */
                PUSH_CHAR(c);
            }
            break;

        case ESCAPE_IN_QUOTED_FIELD:
            /* if (c == '\0') */
            /*  c = '\n'; */

            PUSH_CHAR(c);
            self->state = IN_QUOTED_FIELD;
            break;

        case QUOTE_IN_QUOTED_FIELD:
            /* doublequote - seen a quote in an quoted field */
            if (self->quoting != QUOTE_NONE && c == self->quotechar) {
                /* save "" as " */

                PUSH_CHAR(c);
                self->state = IN_QUOTED_FIELD;
            }
            else if (c == self->delimiter) {
                // End of field. End of line not reached yet

                END_FIELD();
                self->state = START_FIELD;
            }
            else if (c == '\n') {
                END_FIELD();
                END_LINE();
                /* self->state = START_RECORD; */
            }
            else if (c == '\r') {
                END_FIELD();
                self->state = EAT_CRNL;
            }
            else if (!self->strict) {
                PUSH_CHAR(c);
                self->state = IN_FIELD;
            }
            else {
                self->error_msg = (char*) malloc(50);
                sprintf(self->error_msg, "'%c' expected after '%c'",
                        self->delimiter, self->quotechar);
                goto parsingerror;
            }
            break;

        case EAT_COMMENT:
            if (c == '\n') {
                END_LINE();
            } else if (c == '\r') {
                self->state = EAT_CRNL;
            }
            break;

        case EAT_CRNL:
            if (c == '\n') {
                END_LINE();
                /* self->state = START_RECORD; */
            } else if (c == self->delimiter){
                // Handle \r-delimited files
                END_LINE_AND_FIELD_STATE(START_FIELD);
            } else {
                /* \r line terminator */

                /* UGH. we don't actually want to consume the token. fix this later */
                if (end_line(self) < 0) {
                    goto parsingerror;
                }
                self->state = START_RECORD;

                /* HACK, let's try this one again */
                --i; buf--;
                if (line_limit > 0 && self->lines == start_lines + line_limit) {
                    goto linelimit;
                }

            }
            break;

        case EAT_CRNL_NOP: /* inside an ignored comment line */
            self->state = START_RECORD;
            /* \r line terminator -- parse this character again */
            if (c != '\n' && c != self->delimiter) {
                --i;
                --buf;
            }
            break;
        default:
            break;

        }
    }

    _TOKEN_CLEANUP();

    TRACE(("Finished tokenizing input\n"))

    return 0;

parsingerror:
    i++;
    _TOKEN_CLEANUP();

    return -1;

linelimit:
    i++;
    _TOKEN_CLEANUP();

    return 0;
}

/* custom line terminator */
int tokenize_delim_customterm(parser_t *self, size_t line_limit)
{

    int i, start_lines;
    char c;
    char *buf = self->data + self->datapos;

    start_lines = self->lines;

    TRACE(("%s\n", buf));

    for (i = self->datapos; i < self->datalen; ++i)
    {
        // Next character in file
        c = *buf++;

        TRACE(("tokenize_delim_customterm - Iter: %d Char: %c Line %d field_count %d, state %d\n",
               i, c, self->file_lines + 1, self->state));

        switch(self->state) {

        case SKIP_LINE:
//            TRACE(("tokenize_delim_customterm SKIP_LINE %c, state %d\n", c, self->state));
            if (c == self->lineterminator) {
                END_LINE();
            }
            break;

        case START_RECORD:
            // start of record
            if (skip_this_line(self, self->file_lines)) {
                self->state = SKIP_LINE;
                if (c == self->lineterminator) {
                    END_LINE();
                }
                break;
            }
            else if (c == self->lineterminator) {
                // \n\r possible?
                if (self->skip_empty_lines)
                {
                    self->file_lines++;
                }
                else
                {
                    END_LINE();
                }
                break;
            }
            else if (c == self->commentchar) {
                self->state = EAT_LINE_COMMENT;
                break;
            }
            else if (IS_WHITESPACE(c) && c != self->delimiter && self->skip_empty_lines)
            {
                self->state = WHITESPACE_LINE;
                break;
            }
            /* normal character - handle as START_FIELD */
            self->state = START_FIELD;
            /* fallthru */
        case START_FIELD:
            /* expecting field */
            if (c == self->lineterminator) {
                END_FIELD();
                END_LINE();
                /* self->state = START_RECORD; */
            }
            else if (c == self->quotechar &&
                     self->quoting != QUOTE_NONE) {
                /* start quoted field */
                self->state = IN_QUOTED_FIELD;
            }
            else if (c == self->escapechar) {
                /* possible escaped character */
                self->state = ESCAPED_CHAR;
            }
            else if (c == ' ' && self->skipinitialspace)
                /* ignore space at start of field */
                ;
            else if (c == self->delimiter) {
                /* save empty field */
                END_FIELD();
            }
            else if (c == self->commentchar) {
                END_FIELD();
                self->state = EAT_COMMENT;
            }
            else {
                /* begin new unquoted field */
                if (self->quoting == QUOTE_NONNUMERIC)
                    self->numeric_field = 1;

                // TRACE(("pushing %c", c));
                PUSH_CHAR(c);
                self->state = IN_FIELD;
            }
            break;

        case WHITESPACE_LINE: // check if line is whitespace-only
            if (c == self->lineterminator) {
                self->file_lines++;
                self->state = START_RECORD; // ignore empty line
            }
            else if (IS_WHITESPACE(c) && c != self->delimiter)
                ;
            else { // backtrack
                /* We have to use i + 1 because buf has been incremented but not i */
                do {
                    --buf;
                    --i;
                } while (i + 1 > self->datapos && *buf != self->lineterminator);

                if (*buf == self->lineterminator) // reached a newline rather than the beginning
                {
                    ++buf; // move pointer to first char after newline
                    ++i;
                }
                self->state = START_FIELD;
            }
            break;

        case ESCAPED_CHAR:
            /* if (c == '\0') */
            /*  c = '\n'; */

            PUSH_CHAR(c);
            self->state = IN_FIELD;
            break;

        case IN_FIELD:
            /* in unquoted field */
            if (c == self->lineterminator) {
                END_FIELD();
                END_LINE();
                /* self->state = START_RECORD; */
            }
            else if (c == self->escapechar) {
                /* possible escaped character */
                self->state = ESCAPED_CHAR;
            }
            else if (c == self->delimiter) {
                // End of field. End of line not reached yet
                END_FIELD();
                self->state = START_FIELD;
            }
            else if (c == self->commentchar) {
                END_FIELD();
                self->state = EAT_COMMENT;
            }
            else {
                /* normal character - save in field */
                PUSH_CHAR(c);
            }
            break;

        case IN_QUOTED_FIELD:
            /* in quoted field */
            if (c == self->escapechar) {
                /* Possible escape character */
                self->state = ESCAPE_IN_QUOTED_FIELD;
            }
            else if (c == self->quotechar &&
                     self->quoting != QUOTE_NONE) {
                if (self->doublequote) {
                    /* doublequote; " represented by "" */
                    self->state = QUOTE_IN_QUOTED_FIELD;
                }
                else {
                    /* end of quote part of field */
                    self->state = IN_FIELD;
                }
            }
            else {
                /* normal character - save in field */
                PUSH_CHAR(c);
            }
            break;

        case ESCAPE_IN_QUOTED_FIELD:
            PUSH_CHAR(c);
            self->state = IN_QUOTED_FIELD;
            break;

        case QUOTE_IN_QUOTED_FIELD:
            /* doublequote - seen a quote in an quoted field */
            if (self->quoting != QUOTE_NONE && c == self->quotechar) {
                /* save "" as " */

                PUSH_CHAR(c);
                self->state = IN_QUOTED_FIELD;
            }
            else if (c == self->delimiter) {
                // End of field. End of line not reached yet

                END_FIELD();
                self->state = START_FIELD;
            }
            else if (c == self->lineterminator) {
                END_FIELD();
                END_LINE();
                /* self->state = START_RECORD; */
            }
            else if (!self->strict) {
                PUSH_CHAR(c);
                self->state = IN_FIELD;
            }
            else {
                self->error_msg = (char*) malloc(50);
                sprintf(self->error_msg, "'%c' expected after '%c'",
                        self->delimiter, self->quotechar);
                goto parsingerror;
            }
            break;

        case EAT_LINE_COMMENT:
            if (c == self->lineterminator) {
                self->file_lines++;
                self->state = START_RECORD;
            }
            break;

        case EAT_COMMENT:
            if (c == self->lineterminator) {
                END_LINE();
            }
            break;

        default:
            break;

        }
    }

    _TOKEN_CLEANUP();

    TRACE(("Finished tokenizing input\n"))

    return 0;

parsingerror:
    i++;
    _TOKEN_CLEANUP();

    return -1;

linelimit:
    i++;
    _TOKEN_CLEANUP();

    return 0;
}

int tokenize_whitespace(parser_t *self, size_t line_limit)
{
    int i, start_lines;
    char c;
    char *buf = self->data + self->datapos;

    start_lines = self->lines;

    TRACE(("%s\n", buf));

    for (i = self->datapos; i < self->datalen; ++i)
    {
        // Next character in file
        c = *buf++;

        TRACE(("tokenize_whitespace - Iter: %d Char: %c Line %d field_count %d, state %d\n",
               i, c, self->file_lines + 1, self->state));

        switch(self->state) {
        case SKIP_LINE:
//            TRACE(("tokenize_whitespace SKIP_LINE %c, state %d\n", c, self->state));
            if (c == '\n') {
                END_LINE();
            } else if (c == '\r') {
                self->file_lines++;
                self->state = EAT_CRNL_NOP;
            }
            break;

        case WHITESPACE_LINE:
            if (c == '\n') {
                self->file_lines++;
                self->state = START_RECORD;
                break;
            }
            else if (c == '\r') {
                self->file_lines++;
                self->state = EAT_CRNL_NOP;
                break;
            }
            // fall through

        case EAT_WHITESPACE:
            if (c == '\n') {
                END_LINE();
                self->state = START_RECORD;
                break;
            } else if (c == '\r') {
                self->state = EAT_CRNL;
                break;
            } else if (!IS_WHITESPACE(c)) {
                // END_FIELD();
                self->state = START_FIELD;
                // Fall through to subsequent state
            } else {
                // if whitespace char, keep slurping
                break;
            }

        case START_RECORD:
            // start of record
            if (skip_this_line(self, self->file_lines)) {
                self->state = SKIP_LINE;
                if (c == '\n') {
                    END_LINE();
                }
                break;
            } else  if (c == '\n') {
                if (self->skip_empty_lines)
                // \n\r possible?
                {
                    self->file_lines++;
                }
                else
                {
                    END_LINE();
                }
                break;
            } else if (c == '\r') {
                if (self->skip_empty_lines)
                {
                    self->file_lines++;
                    self->state = EAT_CRNL_NOP;
                }
                else
                    self->state = EAT_CRNL;
                break;
            } else if (IS_WHITESPACE(c)) {
                /*if (self->skip_empty_lines)
                    self->state = WHITESPACE_LINE;
                    else*/
                    self->state = EAT_WHITESPACE;
                break;
            } else if (c == self->commentchar) {
                self->state = EAT_LINE_COMMENT;
                break;
            } else {
                /* normal character - handle as START_FIELD */
                self->state = START_FIELD;
            }
            /* fallthru */
        case START_FIELD:
            /* expecting field */
            if (c == '\n') {
                END_FIELD();
                END_LINE();
                /* self->state = START_RECORD; */
            } else if (c == '\r') {
                END_FIELD();
                self->state = EAT_CRNL;
            }
            else if (c == self->quotechar &&
                     self->quoting != QUOTE_NONE) {
                /* start quoted field */
                self->state = IN_QUOTED_FIELD;
            }
            else if (c == self->escapechar) {
                /* possible escaped character */
                self->state = ESCAPED_CHAR;
            }
            /* else if (c == ' ' && self->skipinitialspace) */
            /*     /\* ignore space at start of field *\/ */
            /*     ; */
            else if (IS_WHITESPACE(c)) {
                self->state = EAT_WHITESPACE;
            }
            else if (c == self->commentchar) {
                END_FIELD();
                self->state = EAT_COMMENT;
            }
            else {
                /* begin new unquoted field */
                if (self->quoting == QUOTE_NONNUMERIC)
                    self->numeric_field = 1;

                // TRACE(("pushing %c", c));
                PUSH_CHAR(c);
                self->state = IN_FIELD;
            }
            break;

        case EAT_LINE_COMMENT:
            if (c == '\n') {
                self->file_lines++;
                self->state = START_RECORD;
            } else if (c == '\r') {
                self->file_lines++;
                self->state = EAT_CRNL_NOP;
            }
            break;

        case ESCAPED_CHAR:
            /* if (c == '\0') */
            /*  c = '\n'; */

            PUSH_CHAR(c);
            self->state = IN_FIELD;
            break;

        case IN_FIELD:
            /* in unquoted field */
            if (c == '\n') {
                END_FIELD();
                END_LINE();
                /* self->state = START_RECORD; */
            } else if (c == '\r') {
                END_FIELD();
                self->state = EAT_CRNL;
            }
            else if (c == self->escapechar) {
                /* possible escaped character */
                self->state = ESCAPED_CHAR;
            }
            else if (IS_WHITESPACE(c)) {
                // End of field. End of line not reached yet
                END_FIELD();
                self->state = EAT_WHITESPACE;
            }
            else if (c == self->commentchar) {
                END_FIELD();
                self->state = EAT_COMMENT;
            }
            else {
                /* normal character - save in field */
                PUSH_CHAR(c);
            }
            break;

        case IN_QUOTED_FIELD:
            /* in quoted field */
            if (c == self->escapechar) {
                /* Possible escape character */
                self->state = ESCAPE_IN_QUOTED_FIELD;
            }
            else if (c == self->quotechar &&
                     self->quoting != QUOTE_NONE) {
                if (self->doublequote) {
                    /* doublequote; " represented by "" */
                    self->state = QUOTE_IN_QUOTED_FIELD;
                }
                else {
                    /* end of quote part of field */
                    self->state = IN_FIELD;
                }
            }
            else {
                /* normal character - save in field */
                PUSH_CHAR(c);
            }
            break;

        case ESCAPE_IN_QUOTED_FIELD:
            /* if (c == '\0') */
            /*  c = '\n'; */

            PUSH_CHAR(c);
            self->state = IN_QUOTED_FIELD;
            break;

        case QUOTE_IN_QUOTED_FIELD:
            /* doublequote - seen a quote in an quoted field */
            if (self->quoting != QUOTE_NONE && c == self->quotechar) {
                /* save "" as " */

                PUSH_CHAR(c);
                self->state = IN_QUOTED_FIELD;
            }
            else if (IS_WHITESPACE(c)) {
                // End of field. End of line not reached yet

                END_FIELD();
                self->state = EAT_WHITESPACE;
            }
            else if (c == '\n') {
                END_FIELD();
                END_LINE();
                /* self->state = START_RECORD; */
            }
            else if (c == '\r') {
                END_FIELD();
                self->state = EAT_CRNL;
            }
            else if (!self->strict) {
                PUSH_CHAR(c);
                self->state = IN_FIELD;
            }
            else {
                self->error_msg = (char*) malloc(50);
                sprintf(self->error_msg, "'%c' expected after '%c'",
                        self->delimiter, self->quotechar);
                goto parsingerror;
            }
            break;

        case EAT_CRNL:
            if (c == '\n') {
                END_LINE();
                /* self->state = START_RECORD; */
            } else if (IS_WHITESPACE(c)){
                // Handle \r-delimited files
                END_LINE_STATE(EAT_WHITESPACE);
            } else {
                /* XXX
                 * first character of a new record--need to back up and reread
                 * to handle properly...
                 */
                i--; buf--; /* back up one character (HACK!) */
                END_LINE_STATE(START_RECORD);
            }
            break;

        case EAT_CRNL_NOP: // inside an ignored comment line
            self->state = START_RECORD;
            /* \r line terminator -- parse this character again */
            if (c != '\n' && c != self->delimiter) {
                --i;
                --buf;
            }
            break;

        case EAT_COMMENT:
            if (c == '\n') {
                END_LINE();
            } else if (c == '\r') {
                self->state = EAT_CRNL;
            }
            break;

        default:
            break;


        }

    }

    _TOKEN_CLEANUP();

    TRACE(("Finished tokenizing input\n"))

    return 0;

parsingerror:
    i++;
    _TOKEN_CLEANUP();

    return -1;

linelimit:
    i++;
    _TOKEN_CLEANUP();

    return 0;
}

static int parser_handle_eof(parser_t *self) {
    TRACE(("handling eof, datalen: %d, pstate: %d\n", self->datalen, self->state))
    if (self->datalen == 0 && (self->state != START_RECORD)) {
        // test cases needed here
        // TODO: empty field at end of line
        TRACE(("handling eof\n"));

        if (self->state == IN_FIELD || self->state == START_FIELD) {
            if (end_field(self) < 0)
                return -1;
        } else if (self->state == QUOTE_IN_QUOTED_FIELD) {
            if (end_field(self) < 0)
                return -1;
        } else if (self->state == IN_QUOTED_FIELD) {
            self->error_msg = (char*) malloc(100);
            sprintf(self->error_msg, "EOF inside string starting at line %d",
                    self->file_lines);
            return -1;
        }

        if (end_line(self) < 0)
            return -1;

        return 0;
    }
    else if (self->datalen == 0 && (self->state == START_RECORD)) {
        return 0;
    }

    return -1;
}

void debug_print_parser(parser_t *self) {
    int line;

    for (line = 0; line < self->lines; ++line)
    {
        printf("(Parsed) Line %d: ", line);

#ifdef TBD
        for (j = 0; j < self->line_fields[j]; ++j)
        {
            token = self->words[j + self->line_start[line]];
            printf("%s ", token);
        }
        printf("\n");
#endif
    }
}

/*
  nrows : number of rows to tokenize (or until reach EOF)
  all : tokenize all the data vs. certain number of rows
 */

int _tokenize_helper(parser_t *self, size_t nrows, int all) {
    parser_op tokenize_bytes;

    int status = 0;
    int start_lines = self->lines;

    if (self->delim_whitespace) {
        tokenize_bytes = tokenize_whitespace;
    } else if (self->lineterminator == '\0') {
        tokenize_bytes = tokenize_delimited;
    } else {
        tokenize_bytes = tokenize_delim_customterm;
    }

    if (self->state == FINISHED) {
        return 0;
    }

    TRACE(("_tokenize_helper: Asked to tokenize %d rows, datapos=%d, datalen=%d\n", \
           (int) nrows, self->datapos, self->datalen));

    while (1) {
        if (!all && self->lines - start_lines >= nrows)
            break;

        if (self->datapos == self->datalen) {
            status = parser_buffer_bytes(self, self->chunksize);

            if (status == REACHED_EOF) {
                // close out last line
                status = parser_handle_eof(self);
                self->state = FINISHED;
                break;
            } else if (status != 0) {
                return status;
            }
        }

        TRACE(("_tokenize_helper: Trying to process %d bytes, datalen=%d, datapos= %d\n",
               self->datalen - self->datapos, self->datalen, self->datapos));
        /* TRACE(("sourcetype: %c, status: %d\n", self->sourcetype, status)); */

        status = tokenize_bytes(self, nrows);

        /* debug_print_parser(self); */

        if (status < 0) {
            // XXX
            TRACE(("_tokenize_helper: Status %d returned from tokenize_bytes, breaking\n",
                   status));
            status = -1;
            break;
        }
    }
    TRACE(("leaving tokenize_helper\n"));
    return status;
}

int tokenize_nrows(parser_t *self, size_t nrows) {
    int status = _tokenize_helper(self, nrows, 0);
    return status;
}

int tokenize_all_rows(parser_t *self) {
    int status = _tokenize_helper(self, -1, 1);
    return status;
}

TCL_RESULT tacsv_read_cmd(ClientData clientdata, Tcl_Interp *ip,
                              int objc, Tcl_Obj *const objv[])
{
    parser_t *parser;
    int i, mode, opt, len, ival, nrows;
    char *s;
    TCL_RESULT res;
    Tcl_Obj **objs;
    Tcl_Channel chan;
    static const char *switches[] = {
        "-comment", "-delimiter", "-doublequote", "-escape",
        "-ignorerrors", "-nrows", "-quote", "-quoting",
        "-skipblanklines", "-skipleadingspace", "-skiprows",
        "-startline", "-terminator",
        NULL
    };
    enum switches_e {
        CSV_COMMENT, CSV_DELIMITER, CSV_DOUBLEQUOTE, CSV_ESCAPE,
        CSV_IGNOREERRORS, CSV_NROWS, CSV_QUOTE, CSV_QUOTING,
        CSV_SKIPBLANKLINES, CSV_SKIPLEADINGSPACE, CSV_SKIPROWS,
        CSV_STARTLINE, CSV_TERMINATOR,
    };

    if (objc < 2) {
	Tcl_WrongNumArgs(ip, 1, objv, "CHANNEL");
	return TCL_ERROR;
    }
        
    chan = Tcl_GetChannel(ip, Tcl_GetString(objv[objc-1]), &mode);
    if (chan == NULL)
        return TCL_ERROR;

    parser = parser_new();
    parser->chunksize = 256*1024; /* TBD - chunksize */
    parser_set_default_options(parser);
    parser_init(parser);
    parser->chan = chan;

    nrows = 0;
    res = TCL_ERROR;
    for (i = 1; i < objc-1; i += 2) {
	if (Tcl_GetIndexFromObj(ip, objv[i], switches, "option", 0, &opt)
            != TCL_OK)
            goto vamoose;
        if ((i+1) >= (objc-1)) {
            Tcl_SetResult(ip, "Missing argument for option", TCL_STATIC);
            goto vamoose;
        }
        s = Tcl_GetStringFromObj(objv[i+1], &len);

        switch ((enum switches_e) opt) {
        case CSV_COMMENT:
            if (len > 1)
                goto invalid_option_value;
            parser->commentchar = *s; /* '\0' -> No comment char */
            break;
        case CSV_DELIMITER:
            if (len != 1)
                goto invalid_option_value;
            parser->delimiter = *s;
            break;
        case CSV_ESCAPE:
            if (len > 1)
                goto invalid_option_value;
            parser->escapechar = *s; /* \0 -> no escape char */
            break;
        case CSV_NROWS:
            res = Tcl_GetIntFromObj(ip, objv[i+1], &nrows);
            if (res != TCL_OK)
                goto invalid_option_value;
            break;
        case CSV_QUOTE:
            if (len > 1)
                goto invalid_option_value;
            parser->quotechar = *s;
            break;
        case CSV_QUOTING:
            if (!strcmp(s, "all"))
                parser->quoting = QUOTE_ALL;
            else if (!strcmp(s, "minimal"))
                parser->quoting = QUOTE_MINIMAL;
            else if (!strcmp(s, "nonnumeric"))
                parser->quoting = QUOTE_NONNUMERIC;
            else if (!strcmp(s, "none"))
                parser->quoting = QUOTE_NONE;
            else
                goto invalid_option_value;
            break;
        case CSV_SKIPROWS:
            res = Tcl_ListObjGetElements(ip, objv[i+1], &len, &objs);
            if (res != TCL_OK)
                goto vamoose;
            else {
                int j;
                Tcl_WideInt wval;
                for (j = 0; j < len; ++j) {
                    res = Tcl_GetWideIntFromObj(ip, objs[j], &wval);
                    if (res != TCL_OK)
                        goto vamoose;
                    if (wval < 0)
                        goto invalid_option_value;
                    parser_add_skiprow(parser, wval);
                }
            }
            break;
        case CSV_STARTLINE:
            res = Tcl_GetIntFromObj(ip, objv[i+1], &ival);
            if (res != TCL_OK)
                goto invalid_option_value;
            parser_set_skipfirstnrows(parser, ival);
            break;
        case CSV_TERMINATOR:
            if (len != 1)
                goto invalid_option_value;
            parser->lineterminator = *s;
            break;
        default:
            res = Tcl_GetBooleanFromObj(ip, objv[i+1], &ival);
            if (res != TCL_OK)
                goto invalid_option_value;
            switch ((enum switches_e) opt) {
            case CSV_DOUBLEQUOTE:
                parser->doublequote = ival;
                break;
            case CSV_IGNOREERRORS:
                parser->error_bad_lines = ival;
                break;
            case CSV_SKIPBLANKLINES:
                parser->skip_empty_lines = ival;
                break;
            case CSV_SKIPLEADINGSPACE:
                parser->skipinitialspace = ival;
                break;
            }
            break;
        }
    }
    
    /* Note res == TCL_ERROR at this point */
    if (nrows) { 
        if (tokenize_nrows(parser, nrows) == 0)
            res = TCL_OK;
    } else {
        if (tokenize_all_rows(parser) == 0)
            res = TCL_OK;
    }

    if (res == TCL_OK)
        Tcl_SetObjResult(ip, parser->rowsObj);
    
vamoose: /* res should contain status */
    parser_free(parser);
    return res;

invalid_option_value: /* objv[i] should be the invalid option */
    Tcl_SetObjResult(ip, Tcl_ObjPrintf("Invalid value for option %s", objv[i]));
    res = TCL_ERROR;
    goto vamoose;
}
