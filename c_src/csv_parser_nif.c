#include <string.h>
#include "erl_nif.h"
#include "../deps/libcsv/csv.h"

typedef int bool;
#define true 1
#define false 0

#define MAX_PARSE_SIZE 16
#define MAX_ROWS_PER_BATCH MAX_PARSE_SIZE + 1

#define min( a, b ) ( ((a) < (b)) ? (a) : (b) )

struct column {
  char *data_ptr;
  int allocated_size;
  int data_size;
};

struct row_buffer {
  struct column *cols_ptr;
  int allocated_n;
  int cols_used;
};

struct out_buffer {
  ERL_NIF_TERM rows[MAX_ROWS_PER_BATCH];
  int row_n;
};

struct csv_buffer {
  char *data_ptr;
  int size;
  int consumed;
};

struct capture {
  int *indexes_ptr;
  int size;
};

struct state {
  struct csv_parser parser;
  struct row_buffer row_buffer;
  struct csv_buffer csv_buffer;
  struct capture capture;
};

struct callback_state {
  struct out_buffer out_buffer;
  struct row_buffer *row_buffer_ptr;
  struct capture *capture_ptr;
  ErlNifEnv* env_ptr;
};

struct csv_chunk {
  char *data_ptr;
  int size;
};

ErlNifResourceType* state_type;

void ensure_column_size(struct column *column_ptr, int size) {
  int new_size;
  if (column_ptr->data_ptr == NULL || column_ptr->allocated_size < size) {
    new_size = ((size + 100) / 100) * 100; // Correcto?
    if (column_ptr->data_ptr != NULL) {
      enif_free(column_ptr->data_ptr);
    }
    column_ptr->data_ptr = enif_alloc(new_size);
    column_ptr->allocated_size = new_size;
  }
}

struct column empty_column() {
  struct column col;
  col.data_ptr = NULL;
  col.allocated_size = 0;
  col.data_size = 0;
  return col;
}

bool is_output_column(struct callback_state* cb_state_ptr, int col_i) {
  int i;

  if (cb_state_ptr->capture_ptr->indexes_ptr == NULL) {
    return true;
  } else {
    for (i = 0; i < cb_state_ptr->capture_ptr->size; i++) {
      if (col_i == cb_state_ptr->capture_ptr->indexes_ptr[i]) {
        return true;
      }
    }
    return false;
  }
}

void ensure_row_buffer_space(struct row_buffer *row_buffer_ptr) {
  struct column *new_cols_ptr;
  int new_allocated_n;
  int i;
  if (row_buffer_ptr->cols_used == row_buffer_ptr->allocated_n) {
    new_allocated_n = row_buffer_ptr->allocated_n + 5;
    new_cols_ptr = enif_alloc(sizeof(struct column) * new_allocated_n);
    memcpy(new_cols_ptr,
           row_buffer_ptr->cols_ptr,
           sizeof(struct column) * row_buffer_ptr->allocated_n);
    for (i = row_buffer_ptr->allocated_n; i < new_allocated_n; i++) {
      new_cols_ptr[i] = empty_column();
    }
    enif_free(row_buffer_ptr->cols_ptr);
    row_buffer_ptr->cols_ptr = new_cols_ptr;
    row_buffer_ptr->allocated_n = new_allocated_n;
  }
}

static void add_value(void *data_ptr, int size,
                      struct callback_state* cb_state_ptr)
{
  struct row_buffer *row_buffer_ptr = cb_state_ptr->row_buffer_ptr;
  struct column *column_ptr;
  ensure_row_buffer_space(row_buffer_ptr);
  if (is_output_column(cb_state_ptr, row_buffer_ptr->cols_used)) {
    column_ptr = &row_buffer_ptr->cols_ptr[row_buffer_ptr->cols_used];
    ensure_column_size(column_ptr, size);
    column_ptr->data_size = size;
    memcpy(column_ptr->data_ptr, data_ptr, size);
  }
  row_buffer_ptr->cols_used++;
}

static void add_row(struct callback_state* cb_state_ptr)
{
  struct out_buffer *out_buffer_ptr = &(cb_state_ptr->out_buffer);
  struct row_buffer *row_buffer_ptr = cb_state_ptr->row_buffer_ptr;
  ErlNifEnv* env_ptr = cb_state_ptr->env_ptr;
  ERL_NIF_TERM out_cols[row_buffer_ptr->cols_used];
  int col_i;
  int out_col_i;
  int cols_used = row_buffer_ptr->cols_used;
  if (out_buffer_ptr->row_n < MAX_ROWS_PER_BATCH) {
    out_col_i = 0;
    for (col_i = 0; col_i < cols_used; col_i++) {
      if (is_output_column(cb_state_ptr, col_i)) {
        out_cols[out_col_i] =
          enif_make_string_len(env_ptr,
                               row_buffer_ptr->cols_ptr[col_i].data_ptr,
                               row_buffer_ptr->cols_ptr[col_i].data_size,
                               ERL_NIF_LATIN1);
        out_col_i++;
      }
    }
    row_buffer_ptr->cols_used = 0;
    out_buffer_ptr->rows[out_buffer_ptr->row_n] =
      enif_make_list_from_array(env_ptr, out_cols, out_col_i);
    out_buffer_ptr->row_n++;
  }
}

void column_callback (void *data_ptr, size_t size, void *cb_state_void_ptr)
{
  add_value(data_ptr,
            size,
            (struct callback_state*) cb_state_void_ptr);
}

void row_callback (int c, void *cb_state_void_ptr)
{
  add_row((struct callback_state*) cb_state_void_ptr);
}

static ERL_NIF_TERM make_output(struct callback_state *cb_state_ptr)
{
  ERL_NIF_TERM ret;
  ErlNifEnv* env_ptr = cb_state_ptr->env_ptr;
  struct out_buffer out_buffer = cb_state_ptr->out_buffer;
  ret = enif_make_list_from_array(env_ptr, out_buffer.rows, out_buffer.row_n);
  out_buffer.row_n = 0;
  return ret;
}

void init_row_buffer(struct row_buffer *row_buffer_ptr)
{
  row_buffer_ptr->cols_ptr = NULL;
  row_buffer_ptr->allocated_n = 0;
  row_buffer_ptr->cols_used = 0;
}

void init_csv_buffer(struct csv_buffer *csv_buffer_ptr)
{
  csv_buffer_ptr->data_ptr = NULL;
  csv_buffer_ptr->size = 0;
}

void init_capture(struct capture *capture_ptr) {
  capture_ptr->indexes_ptr = NULL;
}

static struct state* init_state()
{
  struct state* state_ptr = enif_alloc_resource(state_type,
                                                sizeof(struct state));
  if (state_ptr != NULL) {
    init_row_buffer(&state_ptr->row_buffer);
    init_csv_buffer(&state_ptr->csv_buffer);
    init_capture(&state_ptr->capture);
  }
  return state_ptr;
}

static bool extract_capture_list(ErlNifEnv* env_ptr, ERL_NIF_TERM list,
                                 int len, int *dst_ptr)
{
  int i;
  ERL_NIF_TERM head;
  ERL_NIF_TERM tail;
  int item;

  tail = list;
  for (i = 0; i < len; i++) {
    if (!enif_get_list_cell(env_ptr, tail, &head, &tail)) {
      return false;
    }
    if (!enif_get_int(env_ptr, head, &item)) {
      return false;
    }
    if (item < 0) {
      return false;
    }
    dst_ptr[i] = item;
  }
  return true;
}

static bool update_capture(ErlNifEnv* env_ptr, struct state *state_ptr,
                           ERL_NIF_TERM list) {
  unsigned len;
  int *indexes_new_ptr;
  struct capture *capture_ptr;

  if (!enif_get_list_length(env_ptr, list, &len)) {
    return false;
  } else {
    indexes_new_ptr = enif_alloc(len * sizeof(int));
    if (!extract_capture_list(env_ptr, list, len, indexes_new_ptr)) {
      enif_free(indexes_new_ptr);
      return false;
    } else {
      capture_ptr = &(state_ptr->capture);
      enif_free(capture_ptr->indexes_ptr);
      capture_ptr->indexes_ptr = indexes_new_ptr;
      capture_ptr->size = len;
      return true;
    }
  }
}

static void init_callback_state(struct callback_state *cb_state_ptr,
                                ErlNifEnv* env_ptr, struct state *state_ptr)
{
  cb_state_ptr->env_ptr = env_ptr;
  cb_state_ptr->out_buffer.row_n = 0;
  cb_state_ptr->row_buffer_ptr = &(state_ptr->row_buffer);
  cb_state_ptr->capture_ptr = &(state_ptr->capture);
}

bool is_csv_buffer_empty(struct csv_buffer *csv_buffer_ptr)
{
  if (csv_buffer_ptr->data_ptr == NULL ||
      csv_buffer_ptr->size == 0 ||
      csv_buffer_ptr->consumed >= csv_buffer_ptr->size) {
    return true;
  } else {
    return false;
  }
}

void get_csv_chunk(struct csv_chunk *chunk_ptr, struct state *state_ptr,
                   int max_len)
{
  struct csv_buffer *csv_buffer_ptr;
  int bytes_read;

  csv_buffer_ptr = &state_ptr->csv_buffer;
  if (is_csv_buffer_empty(csv_buffer_ptr)) {
    chunk_ptr->size = 0;
  } else {
    bytes_read = min(csv_buffer_ptr->size - csv_buffer_ptr->consumed,
                     max_len);
    chunk_ptr->size = bytes_read;
    chunk_ptr->data_ptr = csv_buffer_ptr->data_ptr + csv_buffer_ptr->consumed;
    csv_buffer_ptr->consumed += bytes_read;
  }
}

ERL_NIF_TERM ok_tuple(ErlNifEnv* env_ptr, ERL_NIF_TERM term)
{
  ERL_NIF_TERM ok_atom = enif_make_atom(env_ptr, "ok");
  return enif_make_tuple2(env_ptr, ok_atom, term);
}

ERL_NIF_TERM error_tuple(ErlNifEnv* env_ptr, ERL_NIF_TERM term)
{
  ERL_NIF_TERM error_atom = enif_make_atom(env_ptr, "error");
  return enif_make_tuple2(env_ptr, error_atom, term);
}

ERL_NIF_TERM error(ErlNifEnv* env_ptr, const char* reason)
{
  return error_tuple(env_ptr, enif_make_string(env_ptr,
                                               reason,
                                               ERL_NIF_LATIN1));
}

ERL_NIF_TERM error2(ErlNifEnv* env_ptr, const char* reason1,
                    const char* reason2)
{
  return error_tuple(env_ptr,
                     enif_make_tuple2(env_ptr,
                                      enif_make_string(env_ptr,
                                                       reason1,
                                                       ERL_NIF_LATIN1),
                                      enif_make_string(env_ptr,
                                                       reason2,
                                                       ERL_NIF_LATIN1)));
}

static ERL_NIF_TERM init(ErlNifEnv* env_ptr, int argc,
                         const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM resource;
  struct state* state_ptr;
  struct csv_parser *parser_ptr;

  state_ptr = init_state();
  if (state_ptr == NULL) {
    return error(env_ptr, "init_state failed");
  }
  parser_ptr = &(state_ptr->parser);
  if (csv_init(parser_ptr, 0) != 0) {
    return error(env_ptr, "csv_init failed");
  }
  resource = enif_make_resource(env_ptr, state_ptr);
  enif_release_resource(state_ptr);
  return ok_tuple(env_ptr, resource);
}

static ERL_NIF_TERM close(ErlNifEnv* env_ptr, int argc,
                          const ERL_NIF_TERM argv[])
{
  struct state *state_ptr;
  struct csv_parser *parser_ptr;
  struct callback_state cb_state;

  if (argc != 1) {
    return enif_make_badarg(env_ptr);
  }
  if (!enif_get_resource(env_ptr, argv[0], state_type, (void**) &state_ptr)) {
    return enif_make_badarg(env_ptr);
  }
  parser_ptr = &(state_ptr->parser);

  init_callback_state(&cb_state, env_ptr, state_ptr);
  if (csv_fini(parser_ptr, column_callback, row_callback, &cb_state) != 0) {
    return error(env_ptr, "csv_fini failed");
  } else {
    return ok_tuple(env_ptr, make_output(&cb_state));
  }
}

static ERL_NIF_TERM feed(ErlNifEnv* env_ptr, int argc,
                         const ERL_NIF_TERM argv[])
{
  struct state *state_ptr;
  ErlNifBinary csv_bin;
  struct csv_buffer *csv_buffer_ptr;

  if (argc != 2) {
     return enif_make_badarg(env_ptr);
  }
  if (!enif_get_resource(env_ptr, argv[0], state_type, (void**) &state_ptr)) {
    return enif_make_badarg(env_ptr);
  }
  if (!enif_inspect_binary(env_ptr, argv[1], &csv_bin)) {
    return enif_make_badarg(env_ptr);
  }

  csv_buffer_ptr = &state_ptr->csv_buffer;
  if (is_csv_buffer_empty(csv_buffer_ptr) == false) {
    return error(env_ptr, "csv buffer not empty");
  } else {
    csv_buffer_ptr->data_ptr = enif_alloc(csv_bin.size);
    if (csv_buffer_ptr->data_ptr == NULL) {
      return error(env_ptr, "could not allocate csv buffer");
    } else {
      memcpy(csv_buffer_ptr->data_ptr, csv_bin.data, csv_bin.size);
      csv_buffer_ptr->size = csv_bin.size;
      csv_buffer_ptr->consumed = 0;
      return enif_make_atom(env_ptr, "ok");
    }
  }
}

static ERL_NIF_TERM set_capture(ErlNifEnv* env_ptr, int argc,
                                const ERL_NIF_TERM argv[])
{
  struct state *state_ptr;

  if (argc != 2) {
    return enif_make_badarg(env_ptr);
  }
  if (!enif_get_resource(env_ptr, argv[0], state_type, (void**) &state_ptr)) {
    return enif_make_badarg(env_ptr);
  }
  if (!update_capture(env_ptr, state_ptr, argv[1])) {
    return enif_make_badarg(env_ptr);
  }

  return enif_make_atom(env_ptr, "ok");
}

static ERL_NIF_TERM parse_one_row(ErlNifEnv* env_ptr, int argc,
                                  const ERL_NIF_TERM argv[])
{
  struct state *state_ptr;
  struct csv_parser *parser_ptr;
  struct callback_state cb_state;
  struct csv_chunk chunk;

  if (argc != 1) {
    return enif_make_badarg(env_ptr);
  }
  if (!enif_get_resource(env_ptr, argv[0], state_type, (void**) &state_ptr)) {
    return enif_make_badarg(env_ptr);
  }
  parser_ptr = &(state_ptr->parser);
  init_callback_state(&cb_state, env_ptr, state_ptr);

  while (cb_state.out_buffer.row_n == 0) {
    get_csv_chunk(&chunk, state_ptr, 1);
    if (chunk.size == 0) {
      return enif_make_tuple2(env_ptr,
                              enif_make_atom(env_ptr, "error"),
                              enif_make_atom(env_ptr, "eob"));
    } else {
      if (csv_parse(parser_ptr, chunk.data_ptr, (size_t) chunk.size,
                    column_callback, row_callback, &cb_state) != chunk.size) {
        return error2(env_ptr,
                      "csv_parse failed",
                      csv_strerror(csv_error(parser_ptr)));
      }
    }
  }
  return ok_tuple(env_ptr, make_output(&cb_state));
}

static ERL_NIF_TERM parse(ErlNifEnv* env_ptr, int argc,
                          const ERL_NIF_TERM argv[])
{
  struct state *state_ptr;
  struct csv_parser *parser_ptr;
  struct callback_state cb_state;
  struct csv_chunk chunk;

  if (argc != 1) {
    return enif_make_badarg(env_ptr);
  }
  if (!enif_get_resource(env_ptr, argv[0], state_type, (void**) &state_ptr)) {
    return enif_make_badarg(env_ptr);
  }
  parser_ptr = &(state_ptr->parser);

  get_csv_chunk(&chunk, state_ptr, MAX_PARSE_SIZE);
  if (chunk.size == 0) {
    return enif_make_tuple2(env_ptr,
                            enif_make_atom(env_ptr, "error"),
                            enif_make_atom(env_ptr, "eob"));
  } else {
    init_callback_state(&cb_state, env_ptr, state_ptr);
    if (csv_parse(parser_ptr, chunk.data_ptr, (size_t) chunk.size,
                  column_callback, row_callback, &cb_state) != chunk.size) {
      return error2(env_ptr,
                    "csv_parse failed",
                    csv_strerror(csv_error(parser_ptr)));
    } else {
      return ok_tuple(env_ptr, make_output(&cb_state));
    }
  }
}

static ErlNifFunc nif_funcs[] =
  {
    {"init", 0, init},
    {"close", 1, close},
    {"feed", 2, feed},
    {"set_capture", 2, set_capture},
    {"parse_one_row", 1, parse_one_row},
    {"parse", 1, parse},
  };

void csv_buffer_dtor(struct csv_buffer *csv_buffer_ptr) {
  if (csv_buffer_ptr->data_ptr != NULL) {
    enif_free(csv_buffer_ptr->data_ptr);
  }
}

void column_dtor(struct column *column_ptr) {
  if (column_ptr->data_ptr != NULL) {
    enif_free(column_ptr->data_ptr);
  }
}

void row_buffer_dtor(struct row_buffer *row_buffer_ptr) {
  int i;
  for (i = 0; i < row_buffer_ptr->allocated_n; i++) {
    column_dtor(&row_buffer_ptr->cols_ptr[i]);
  }
  enif_free(row_buffer_ptr->cols_ptr);
}

void capture_dtor(struct capture *capture_ptr) {
  if (capture_ptr->indexes_ptr != NULL) {
    enif_free(capture_ptr->indexes_ptr);
  }
}

void state_dtor(ErlNifEnv* env_ptr, void* obj_ptr)
{
  struct state* state_ptr = (struct state*) obj_ptr;
  struct csv_parser *parser_ptr = &(state_ptr->parser);
  struct csv_buffer *csv_buffer_ptr = &(state_ptr->csv_buffer);
  struct row_buffer *row_buffer_ptr = &(state_ptr->row_buffer);
  struct capture *capture_ptr = &(state_ptr->capture);
  csv_free(parser_ptr);
  csv_buffer_dtor(csv_buffer_ptr);
  row_buffer_dtor(row_buffer_ptr);
  capture_dtor(capture_ptr);
}

static int load(ErlNifEnv* env_ptr, void** priv, ERL_NIF_TERM info)
{
  // Use ERL_NIF_RT_TAKEOVER?
  int flags = ERL_NIF_RT_CREATE;
  state_type = enif_open_resource_type(env_ptr, NULL, "state",
                                       state_dtor, flags, NULL);
  if (state_type == NULL) {
    return 1;
  } else {
    return 0;
  }
}

ERL_NIF_INIT(csv_parser, nif_funcs, load, NULL, NULL, NULL)
