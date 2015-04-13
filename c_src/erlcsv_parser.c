#include <string.h>
#include "erl_nif.h"
#include "csv.h"

#define INPUT_BUFFER_LEN 4096
#define MAX_COLS 100
#define MAX_ROWS 1000

struct state {
  struct csv_parser parser;
};

struct output_row {
  ErlNifBinary cols[MAX_COLS];
  int col_offset;
};

struct output {
  struct output_row rows[MAX_ROWS];
  int row_offset;
};

ErlNifResourceType* state_type;

static void init_output(struct output *out) {
  (*out).row_offset = 0;
  (*out).rows[0].col_offset = 0;
}

static void add_value(void *s, int size, struct output* out) {
  struct output_row *row = &(*out).rows[ (*out).row_offset ];
  ErlNifBinary *value;
  value = &(*row).cols[ (*row).col_offset++ ];
  enif_alloc_binary(size, value);
  (*value).size = size;
  memcpy((*value).data, s, size);
}

static void add_row(struct output* out) {
  struct output_row *row;
  row = &(*out).rows[ ++(*out).row_offset ];
  (*row).col_offset = 0;
}

void column_callback (void *s, size_t i, void *out_void) {
  struct output *out;
  printf("%.*s\t", i, s);
  out = (struct output*) out_void;
  add_value(s, i, out);
}

void row_callback (int c, void *out_void) {
  struct output *out;
  printf("\tNL\r\n");
  out = (struct output*) out_void;
  add_row(out);
}

static ERL_NIF_TERM output_row(ErlNifEnv* env, struct output_row *row) {
  int cols_n = (*row).col_offset;
  ERL_NIF_TERM out_cols[cols_n];
  int i;
  for (i = 0; i < cols_n; i++) {
    out_cols[i] = enif_make_binary(env, &((*row).cols[i]));
  }
  printf("output_row, cols: %d\r\n", cols_n);
  return enif_make_list_from_array(env, out_cols, cols_n);
}

static ERL_NIF_TERM make_output(ErlNifEnv* env, struct output *out) {
  int rows_n = (*out).row_offset;
  ERL_NIF_TERM out_rows[rows_n];
  int i;
  for (i = 0; i < rows_n; i++) {
    out_rows[i] = output_row(env, &(*out).rows[i]);
  }
  printf("make_output, rows: %d\r\n", rows_n);
  return enif_make_list_from_array(env, out_rows, rows_n);
}

static ERL_NIF_TERM init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret;
  struct state* state_ptr = enif_alloc_resource(state_type,
                                                sizeof(struct state));
  struct csv_parser *parser_ptr = &((*state_ptr).parser);
  csv_init(parser_ptr, 0);
  ret = enif_make_resource(env, parser_ptr);
  enif_release_resource(state_ptr);
  return ret;
}

static ERL_NIF_TERM close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct state *state_ptr;
  struct csv_parser *parser_ptr;
  struct output *out = malloc(sizeof(struct output));

  if (argc != 1) {
    return enif_make_badarg(env);
  }
  if (!enif_get_resource(env, argv[0], state_type, (void**) &state_ptr)) {
    return enif_make_badarg(env);
  }
  parser_ptr = &((*state_ptr).parser);

  init_output(out);
  csv_fini(parser_ptr, column_callback, row_callback, out);
  return make_output(env, out);
}

static ERL_NIF_TERM parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary csv;
  struct state *state_ptr;
  struct csv_parser *parser_ptr;
  struct output *out = malloc(sizeof(struct output));

  if (argc != 2) {
    return enif_make_badarg(env);
  }
  if (!enif_get_resource(env, argv[0], state_type, (void**) &state_ptr)) {
    return enif_make_badarg(env);
  }
  parser_ptr = &((*state_ptr).parser);

  if (!enif_inspect_binary(env, argv[1], &csv)) {
    return enif_make_badarg(env);
  }
  if (csv.size > INPUT_BUFFER_LEN) {
    return enif_make_badarg(env);
  }

  init_output(out);
  if (csv_parse(parser_ptr, csv.data, (size_t) csv.size,
                column_callback, row_callback, out) != csv.size) {
    return enif_make_string(env,
                            csv_strerror(csv_error(parser_ptr)),
                            ERL_NIF_LATIN1);
  }
  return make_output(env, out);
}

static ErlNifFunc nif_funcs[] =
{
  {"init", 0, init},
  {"close", 1, close},
  {"parse", 2, parse},
};


void state_dtor(ErlNifEnv* env, void* obj_ptr)
{
  struct state* state_ptr = (struct state*) obj_ptr;
  struct csv_parser *parser_ptr = &((*state_ptr).parser);
  printf("dtor!!\r\n");
  csv_free(parser_ptr);
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
  // Use ERL_NIF_RT_TAKEOVER?
  int flags = ERL_NIF_RT_CREATE;
  state_type = enif_open_resource_type(env, NULL, "state",
                                       state_dtor, flags, NULL);
  if (state_type == NULL) {
    return 1;
  } else {
    return 0;
  }
}

ERL_NIF_INIT(erlcsv_parser, nif_funcs, load, NULL, NULL, NULL)
