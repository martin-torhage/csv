#include <string.h>
#include "erl_nif.h"
#include "csv.h"

#define INPUT_BUFFER_LEN 4096
#define MAX_COLS 100
#define MAX_ROWS 1000

struct output_row {
  ErlNifBinary cols[MAX_COLS];
  int col_offset;
};

struct output {
  struct output_row rows[MAX_ROWS];
  int row_offset;
};

static void init_output(struct output *out) {
  (*out).row_offset = 0;
}

static ERL_NIF_TERM world(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
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

void cb1 (void *s, size_t i, void *out_void) {
  struct output *out;
  out = (struct output*) out_void;
  add_value(s, i, out);
}

void cb2 (int c, void *out_void) {
  struct output *out;
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
  return enif_make_list_from_array(env, out_cols, cols_n);
}

static ERL_NIF_TERM make_output(ErlNifEnv* env, struct output *out) {
  int rows_n = (*out).row_offset;
  ERL_NIF_TERM out_rows[rows_n];
  int i;
  for (i = 0; i < rows_n; i++) {
    //out_rows[i] = enif_make_binary(env, &(*out).rows[i].cols[0]);
    out_rows[i] = output_row(env, &(*out).rows[i]);
    //out_terms[0] = enif_make_string(env, "Hejpna", ERL_NIF_LATIN1);
  }

  /* out_terms[0] = enif_make_binary(env, &(*out).rows[0].cols[0]); */
  /* out_terms[1] = enif_make_binary(env, &(*out).rows[0].cols[1]); */
  /* out_terms[2] = enif_make_binary(env, &(*out).rows[1].cols[0]); */
  /* out_terms[3] = enif_make_binary(env, &(*out).rows[1].cols[1]); */
  return enif_make_list_from_array(env, out_rows, rows_n);
}

static ERL_NIF_TERM parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary csv;
  struct csv_parser p;
  //int i;
  //char c;
  //ERL_NIF_TERM out_terms[4];
  struct output *out = malloc(sizeof(struct output));

  init_output(out);
  // struct writer_state *state = malloc(sizeof(struct writer_state));

  if(!enif_inspect_binary(env, argv[0], &csv)) return enif_make_badarg(env);
  if(csv.size > INPUT_BUFFER_LEN) return enif_make_badarg(env);

  /* enif_alloc_binary(5 + csv.size, &ret); */
  /* strcpy(ret.data, "HELLO"); */
  /* strcat(ret.data, csv.data); */
  //(*state).put_comma = 0;
  csv_init(&p, 0);
  if (csv_parse(&p, csv.data, (size_t) csv.size, cb1, cb2, out) != csv.size) {
    return enif_make_string(env, csv_strerror(csv_error(&p)), ERL_NIF_LATIN1);
    // return enif_make_badarg(env);
  }
  csv_fini(&p, cb1, cb2, out);
  csv_free(&p);

  //out_terms[0] = enif_make_string(env, "Tjossna", ERL_NIF_LATIN1);
  /* out_terms[1] = enif_make_string(env, "hopssna", ERL_NIF_LATIN1); */
  /* out_terms[2] = enif_make_string(env, "theheh", ERL_NIF_LATIN1); */

  //return enif_make_list_from_array(env, out_terms, 1);
  /* return enif_make_list(env, 3, */
  /*                       enif_make_string(env, "Hej", ERL_NIF_LATIN1), */
  /*                       enif_make_string(env, "hopp", ERL_NIF_LATIN1), */
  /*                       enif_make_string(env, "gummisnopp!", ERL_NIF_LATIN1)); */

  //return enif_make_binary(env, &ret);
  /* add_value("add_value", 9, out); */
  /* add_value("add_value2", 10, out); */
  /* add_row(out); */
  /* add_value("more value", 10, out); */
  /* add_value("more value2", 11, out); */
  return make_output(env, out);
}

static ErlNifFunc nif_funcs[] =
{
  {"world", 0, world},
  {"parse", 1, parse}
};

ERL_NIF_INIT(erlcsv_parser, nif_funcs, NULL, NULL, NULL, NULL)
