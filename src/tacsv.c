/*

Copyright (c) 2012, Lambda Foundry, Inc., except where noted

Incorporates components of WarrenWeckesser/textreader, licensed under 3-clause
BSD

   Low-level ascii-file processing from pandas. Combines some elements from
   Python's built-in csv module and Warren Weckesser's textreader project on
   GitHub. See Python Software Foundation License and BSD licenses for these.

   Heavily adapted for tarray/Tcl
  */

#include "tarray.h"
#include "tacsv.h"

#include <ctype.h>
#include <math.h>
#include <float.h>

KHASH_MAP_INIT_INT64(int64, size_t)

static void free_if_not_null(void **ptr) {
    if (*ptr != NULL) {
        free(*ptr);
        *ptr = NULL;
    }
}

static void *grow_buffer(void *buffer, int length, int *capacity,
                         int space, int elsize, int *error) {
    int cap = *capacity;
    void *newbuffer = buffer;

    // Can we fit potentially nbytes tokens (+ null terminators) in the stream?
    while ( (length + space > cap) && (newbuffer != NULL) ){
        cap = cap? cap << 1 : 2;
        buffer = newbuffer;
        newbuffer = realloc(newbuffer, elsize * cap);
    }

    if (newbuffer == NULL) {
        // realloc failed so don't change *capacity, set *error to errno
        // and return the last good realloc'd buffer so it can be freed
        *error = errno;
        newbuffer = buffer;
    } else {
        // realloc worked, update *capacity and set *error to 0
        // sigh, multiple return values
        *capacity = cap;
        *error = 0;
    }
    return newbuffer;
}

void parser_set_default_options(parser_t *self) {
    self->decimal = '.';
    self->sci = 'E';

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
    self->thousands = '\0';

    self->skipset = NULL;
    self-> skip_first_N_rows = -1;
    self->skip_footer = 0;
}

parser_t* parser_new() {
    return (parser_t*) calloc(1, sizeof(parser_t));
}

int parser_clear_data_buffers(parser_t *self) {
    free_if_not_null((void *)&self->stream);
    free_if_not_null((void *)&self->words);
    free_if_not_null((void *)&self->word_starts);
    free_if_not_null((void *)&self->line_start);
    free_if_not_null((void *)&self->line_fields);
    return 0;
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

    if (parser_clear_data_buffers(self) < 0) {
        status = -1;
    }

    if (self->cb_cleanup != NULL) {
        if (self->cb_cleanup(self->source) < 0) {
            status = -1;
        }
    }

    return status;
}

int parser_init(parser_t *self) {
    int sz;

    /*
      Initialize data buffers
    */

    self->stream = NULL;
    self->words = NULL;
    self->word_starts = NULL;
    self->line_start = NULL;
    self->line_fields = NULL;
    self->error_msg = NULL;
    self->warn_msg = NULL;

    // token stream
    self->stream = (char*) malloc(STREAM_INIT_SIZE * sizeof(char));
    if (self->stream == NULL) {
        parser_cleanup(self);
        return PARSER_OUT_OF_MEMORY;
    }
    self->stream_cap = STREAM_INIT_SIZE;
    self->stream_len = 0;

    // word pointers and metadata
    sz = STREAM_INIT_SIZE / 10;
    sz = sz? sz : 1;
    self->words = (char**) malloc(sz * sizeof(char*));
    self->word_starts = (int*) malloc(sz * sizeof(int));
    self->words_cap = sz;
    self->words_len = 0;

    // line pointers and metadata
    self->line_start = (int*) malloc(sz * sizeof(int));

    self->line_fields = (int*) malloc(sz * sizeof(int));

    self->lines_cap = sz;
    self->lines = 0;
    self->file_lines = 0;

    if (self->stream == NULL || self->words == NULL ||
        self->word_starts == NULL || self->line_start == NULL ||
        self->line_fields == NULL) {

        parser_cleanup(self);

        return PARSER_OUT_OF_MEMORY;
    }

    /* amount of bytes buffered */
    self->datalen = 0;
    self->datapos = 0;

    self->line_start[0] = 0;
    self->line_fields[0] = 0;

    self->pword_start = self->stream;
    self->word_start = 0;

    self->state = START_RECORD;

    self->error_msg = NULL;
    self->warn_msg = NULL;

    self->commentchar = '\0';

    return 0;
}


void parser_free(parser_t *self) {
    // opposite of parser_init
    parser_cleanup(self);
    free(self);
}


static int make_stream_space(parser_t *self, size_t nbytes) {
    int i, status, cap;
    void *orig_ptr, *newptr;

    // Can we fit potentially nbytes tokens (+ null terminators) in the stream?

    /*
      TOKEN STREAM
    */

    orig_ptr = (void *) self->stream;
    TRACE(("\n\nmake_stream_space: nbytes = %zu.  grow_buffer(self->stream...)\n", nbytes))
    self->stream = (char*) grow_buffer((void *) self->stream,
                                        self->stream_len,
                                        &self->stream_cap, nbytes * 2,
                                        sizeof(char), &status);
    TRACE(("make_stream_space: self->stream=%p, self->stream_len = %zu, self->stream_cap=%zu, status=%zu\n",
           self->stream, self->stream_len, self->stream_cap, status))

    if (status != 0) {
        return PARSER_OUT_OF_MEMORY;
    }

    // realloc sets errno when moving buffer?
    if (self->stream != orig_ptr) {
        // uff
        /* TRACE(("Moving word pointers\n")) */

        self->pword_start = self->stream + self->word_start;

        for (i = 0; i < self->words_len; ++i)
        {
            self->words[i] = self->stream + self->word_starts[i];
        }
    }


    /*
      WORD VECTORS
    */

    cap = self->words_cap;
    self->words = (char**) grow_buffer((void *) self->words,
                                       self->words_len,
                                       &self->words_cap, nbytes,
                                       sizeof(char*), &status);
    TRACE(("make_stream_space: grow_buffer(self->self->words, %zu, %zu, %zu, %d)\n",
           self->words_len, self->words_cap, nbytes, status))
    if (status != 0) {
        return PARSER_OUT_OF_MEMORY;
    }


    // realloc took place
    if (cap != self->words_cap) {
        TRACE(("make_stream_space: cap != self->words_cap, nbytes = %d, self->words_cap=%d\n", nbytes, self->words_cap))
        newptr = realloc((void *) self->word_starts, sizeof(int) * self->words_cap);
        if (newptr == NULL) {
            return PARSER_OUT_OF_MEMORY;
        } else {
            self->word_starts = (int*) newptr;
        }
    }


    /*
      LINE VECTORS
    */
    /*
    printf("Line_start: ");

    for (j = 0; j < self->lines + 1; ++j) {
         printf("%d ", self->line_fields[j]);
     }
    printf("\n");

    printf("lines_cap: %d\n", self->lines_cap);
    */
    cap = self->lines_cap;
    self->line_start = (int*) grow_buffer((void *) self->line_start,
                                          self->lines + 1,
                                          &self->lines_cap, nbytes,
                                          sizeof(int), &status);
    TRACE(("make_stream_space: grow_buffer(self->line_start, %zu, %zu, %zu, %d)\n",
           self->lines + 1, self->lines_cap, nbytes, status))
    if (status != 0) {
        return PARSER_OUT_OF_MEMORY;
    }

    // realloc took place
    if (cap != self->lines_cap) {
        TRACE(("make_stream_space: cap != self->lines_cap, nbytes = %d\n", nbytes))
        newptr = realloc((void *) self->line_fields, sizeof(int) * self->lines_cap);
        if (newptr == NULL) {
            return PARSER_OUT_OF_MEMORY;
        } else {
            self->line_fields = (int*) newptr;
        }
    }

    /* TRACE(("finished growing buffers\n")); */

    return 0;
}

static int push_char(parser_t *self, char c) {
    /* TRACE(("pushing %c \n", c)) */
    TRACE(("push_char: self->stream[%zu] = %x, stream_cap=%zu\n", self->stream_len+1, c, self->stream_cap))
    if (self->stream_len >= self->stream_cap) {
        TRACE(("push_char: ERROR!!! self->stream_len(%d) >= self->stream_cap(%d)\n",
               self->stream_len, self->stream_cap))
        self->error_msg = (char*) malloc(64);
        sprintf(self->error_msg, "Buffer overflow caught - possible malformed input file.\n");
        return PARSER_OUT_OF_MEMORY;
    }
    self->stream[self->stream_len++] = c;
    return 0;
}

static int P_INLINE end_field(parser_t *self) {
    // XXX cruft
//    self->numeric_field = 0;
    if (self->words_len >= self->words_cap) {
        TRACE(("end_field: ERROR!!! self->words_len(%zu) >= self->words_cap(%zu)\n", self->words_len, self->words_cap))
        self->error_msg = (char*) malloc(64);
        sprintf(self->error_msg, "Buffer overflow caught - possible malformed input file.\n");
        return PARSER_OUT_OF_MEMORY;
    }

    // null terminate token
    push_char(self, '\0');

    // set pointer and metadata
    self->words[self->words_len] = self->pword_start;

    TRACE(("end_field: Char diff: %d\n", self->pword_start - self->words[0]));

    TRACE(("end_field: Saw word %s at: %d. Total: %d\n",
           self->pword_start, self->word_start, self->words_len + 1))

    self->word_starts[self->words_len] = self->word_start;
    self->words_len++;

    // increment line field count
    self->line_fields[self->lines]++;

    // New field begin in stream
    self->pword_start = self->stream + self->stream_len;
    self->word_start = self->stream_len;

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
    char *msg;

    fields = self->line_fields[self->lines];

    TRACE(("end_line: Line end, nfields: %d\n", fields));

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
    int status;
    size_t bytes_read;

    status = 0;
    self->datapos = 0;
    self->data = self->cb_io(self->source, nbytes, &bytes_read, &status);
    TRACE(("parser_buffer_bytes self->cb_io: nbytes=%zu, datalen: %d, status=%d\n",
           nbytes, bytes_read, status));
    self->datalen = bytes_read;

    if (status != REACHED_EOF && self->data == NULL) {
        self->error_msg = (char*) malloc(200);

        if (status == CALLING_READ_FAILED) {
            sprintf(self->error_msg, ("Calling read(nbytes) on source failed. "
                                      "Try engine='python'."));
        } else {
            sprintf(self->error_msg, "Unknown error in IO callback");
        }
        return -1;
    }

    TRACE(("datalen: %d\n", self->datalen));

    return status;
}


/*

  Tokenization macros and state machine code

*/

//    printf("pushing %c\n", c);

#define PUSH_CHAR(c)                                \
    TRACE(("PUSH_CHAR: Pushing %c, slen= %d, stream_cap=%zu, stream_len=%zu\n", c, slen, self->stream_cap, self->stream_len)) \
    if (slen >= maxstreamsize) {                    \
        TRACE(("PUSH_CHAR: ERROR!!! slen(%d) >= maxstreamsize(%d)\n", slen, maxstreamsize))            \
        self->error_msg = (char*) malloc(100);      \
        sprintf(self->error_msg, "Buffer overflow caught - possible malformed input file.\n"); \
        return PARSER_OUT_OF_MEMORY;                \
    }                                               \
    *stream++ = c;                                  \
    slen++;

// This is a little bit of a hack but works for now

#define END_FIELD()                            \
    self->stream_len = slen;                   \
    if (end_field(self) < 0) {                 \
        goto parsingerror;                     \
    }                                          \
    stream = self->stream + self->stream_len;  \
    slen = self->stream_len;

#define END_LINE_STATE(STATE)                                           \
    self->stream_len = slen;                                            \
    if (end_line(self) < 0) {                                           \
        goto parsingerror;                                              \
    }                                                                   \
    stream = self->stream + self->stream_len;                           \
    slen = self->stream_len;                                            \
    self->state = STATE;                                                \
    if (line_limit > 0 && self->lines == start_lines + line_limit) {    \
        goto linelimit;                                                 \
                                                                        \
    }

#define END_LINE_AND_FIELD_STATE(STATE)                                 \
    self->stream_len = slen;                                            \
    if (end_line(self) < 0) {                                           \
        goto parsingerror;                                              \
    }                                                                   \
    if (end_field(self) < 0) {                                          \
        goto parsingerror;                                              \
    }                                                                   \
    stream = self->stream + self->stream_len;                           \
    slen = self->stream_len;                                            \
    self->state = STATE;                                                \
    if (line_limit > 0 && self->lines == start_lines + line_limit) {    \
        goto linelimit;                                                 \
                                                                        \
    }

#define END_LINE() END_LINE_STATE(START_RECORD)

#define IS_WHITESPACE(c) ((c == ' ' || c == '\t'))

typedef int (*parser_op)(parser_t *self, size_t line_limit);

#define _TOKEN_CLEANUP()                                                \
    self->stream_len = slen;                                            \
    self->datapos = i;                                                  \
    TRACE(("_TOKEN_CLEANUP: datapos: %d, datalen: %d\n", self->datapos, self->datalen));


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
    int i, slen, start_lines;
    long maxstreamsize;
    char c;
    char *stream;
    char *buf = self->data + self->datapos;


    start_lines = self->lines;

    if (make_stream_space(self, self->datalen - self->datapos) < 0) {
        self->error_msg = "out of memory";
        return -1;
    }

    stream = self->stream + self->stream_len;
    slen = self->stream_len;
    maxstreamsize = self->stream_cap;
    TRACE(("%s\n", buf));

    for (i = self->datapos; i < self->datalen; ++i)
    {
        // Next character in file
        c = *buf++;

        TRACE(("tokenize_delimited - Iter: %d Char: 0x%x Line %d field_count %d, state %d\n",
               i, c, self->file_lines + 1, self->line_fields[self->lines],
               self->state));

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
                self->stream_len = slen;
                if (end_line(self) < 0) {
                    goto parsingerror;
                }
                stream = self->stream + self->stream_len;
                slen = self->stream_len;
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

    int i, slen, start_lines;
    long maxstreamsize;
    char c;
    char *stream;
    char *buf = self->data + self->datapos;


    start_lines = self->lines;

    if (make_stream_space(self, self->datalen - self->datapos) < 0) {
        self->error_msg = "out of memory";
        return -1;
    }

    stream = self->stream + self->stream_len;
    slen = self->stream_len;
    maxstreamsize = self->stream_cap;

    TRACE(("%s\n", buf));

    for (i = self->datapos; i < self->datalen; ++i)
    {
        // Next character in file
        c = *buf++;

        TRACE(("tokenize_delim_customterm - Iter: %d Char: %c Line %d field_count %d, state %d\n",
               i, c, self->file_lines + 1, self->line_fields[self->lines],
               self->state));

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
    int i, slen, start_lines;
    long maxstreamsize;
    char c;
    char *stream;
    char *buf = self->data + self->datapos;

    start_lines = self->lines;

    if (make_stream_space(self, self->datalen - self->datapos) < 0) {
        self->error_msg = "out of memory";
        return -1;
    }

    stream = self->stream + self->stream_len;
    slen = self->stream_len;
    maxstreamsize = self->stream_cap;

    TRACE(("%s\n", buf));

    for (i = self->datapos; i < self->datalen; ++i)
    {
        // Next character in file
        c = *buf++;

        TRACE(("tokenize_whitespace - Iter: %d Char: %c Line %d field_count %d, state %d\n",
               i, c, self->file_lines + 1, self->line_fields[self->lines],
               self->state));

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

int parser_consume_rows(parser_t *self, size_t nrows) {
    int i, offset, word_deletions, char_count;

    if (nrows > self->lines) {
        nrows = self->lines;
    }

    /* do nothing */
    if (nrows == 0)
        return 0;

    /* cannot guarantee that nrows + 1 has been observed */
    word_deletions = self->line_start[nrows - 1] + self->line_fields[nrows - 1];
    char_count = (self->word_starts[word_deletions - 1] +
                  strlen(self->words[word_deletions - 1]) + 1);

    TRACE(("parser_consume_rows: Deleting %d words, %d chars\n", word_deletions, char_count));

    /* move stream, only if something to move */
    if (char_count < self->stream_len) {
        memmove((void*) self->stream, (void*) (self->stream + char_count),
                self->stream_len - char_count);
    }
    /* buffer counts */
    self->stream_len -= char_count;

    /* move token metadata */
    for (i = 0; i < self->words_len - word_deletions; ++i) {
        offset = i + word_deletions;

        self->words[i] = self->words[offset] - char_count;
        self->word_starts[i] = self->word_starts[offset] - char_count;
    }
    self->words_len -= word_deletions;

    /* move current word pointer to stream */
    self->pword_start -= char_count;
    self->word_start -= char_count;
    /*
    printf("Line_start: ");
    for (i = 0; i < self->lines + 1; ++i) {
         printf("%d ", self->line_fields[i]);
     }
    printf("\n");
    */
    /* move line metadata */
    for (i = 0; i < self->lines - nrows + 1; ++i)
    {
        offset = i + nrows;
        self->line_start[i] = self->line_start[offset] - word_deletions;

        /* TRACE(("First word in line %d is now %s\n", i, */
        /*        self->words[self->line_start[i]])); */

        self->line_fields[i] = self->line_fields[offset];
    }
    self->lines -= nrows;
    /* self->line_fields[self->lines] = 0; */

    return 0;
}

static size_t _next_pow2(size_t sz) {
    size_t result = 1;
    while (result < sz) result *= 2;
    return result;
}

int parser_trim_buffers(parser_t *self) {
    /*
      Free memory
     */
    size_t new_cap;
    void *newptr;

    /* trim stream */
    new_cap = _next_pow2(self->stream_len) + 1;
    TRACE(("parser_trim_buffers: new_cap = %zu, stream_cap = %zu, lines_cap = %zu\n",
           new_cap, self->stream_cap, self->lines_cap));
    if (new_cap < self->stream_cap) {
        TRACE(("parser_trim_buffers: new_cap < self->stream_cap, calling safe_realloc\n"));
        newptr = realloc((void*) self->stream, new_cap);
        if (newptr == NULL) {
            return PARSER_OUT_OF_MEMORY;
        } else {
            self->stream = newptr;
            self->stream_cap = new_cap;
        }
    }

    /* trim words, word_starts */
    new_cap = _next_pow2(self->words_len) + 1;
    if (new_cap < self->words_cap) {
        TRACE(("parser_trim_buffers: new_cap < self->words_cap\n"));
        newptr = realloc((void*) self->words, new_cap * sizeof(char*));
        if (newptr == NULL) {
            return PARSER_OUT_OF_MEMORY;
        } else {
            self->words = (char**) newptr;
        }
        newptr = realloc((void*) self->word_starts, new_cap * sizeof(int));
        if (newptr == NULL) {
            return PARSER_OUT_OF_MEMORY;
        } else {
            self->word_starts = (int*) newptr;
            self->words_cap = new_cap;
        }
    }

    /* trim line_start, line_fields */
    new_cap = _next_pow2(self->lines) + 1;
    if (new_cap < self->lines_cap) {
        TRACE(("parser_trim_buffers: new_cap < self->lines_cap\n"));
        newptr = realloc((void*) self->line_start, new_cap * sizeof(int));
        if (newptr == NULL) {
            return PARSER_OUT_OF_MEMORY;
        } else {
            self->line_start = (int*) newptr;
        }
        newptr = realloc((void*) self->line_fields, new_cap * sizeof(int));
        if (newptr == NULL) {
            return PARSER_OUT_OF_MEMORY;
        } else {
            self->line_fields = (int*) newptr;
            self->lines_cap = new_cap;
        }
    }

    return 0;
}

void debug_print_parser(parser_t *self) {
    int j, line;
    char *token;

    for (line = 0; line < self->lines; ++line)
    {
        printf("(Parsed) Line %d: ", line);

        for (j = 0; j < self->line_fields[j]; ++j)
        {
            token = self->words[j + self->line_start[line]];
            printf("%s ", token);
        }
        printf("\n");
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

TA_INLINE void uppercase(char *p) {
    for ( ; *p; ++p) *p = toupper(*p);
}

#ifdef NOTNEEDED
int TA_INLINE to_longlong(char *item, Tcl_WideInt *p_value)
{
    char *p_end;

    // Try integer conversion.  We explicitly give the base to be 10. If
    // we used 0, strtoll() would convert '012' to 10, because the leading 0 in
    // '012' signals an octal number in C.  For a general purpose reader, that
    // would be a bug, not a feature.
    *p_value = strtoll(item, &p_end, 10);

    // Allow trailing spaces.
    while (isspace(*p_end)) ++p_end;

    return (errno == 0) && (!*p_end);
}

int to_boolean(const char *item, uint8_t *val) {
    int ival;

    if (Tcl_GetBoolean(NULL, item, &ival) != TCL_ERROR)
        return -1;

    *val = (ival != 0);
    return 0;
}
#endif
