#include <string.h>
#include "erl_nif.h"
#include "csv.h"

#define INPUT_BUFFER_LEN 4096
#define MAX_COLS 100
#define MAX_ROWS 1000

struct row_buffer {
  ErlNifBinary* cols[4096];
  int col_n;
};

struct out_buffer {
  ERL_NIF_TERM rows[4096];
  int row_n;
};

struct state {
  struct csv_parser parser;
  struct row_buffer row_buffer;
};

struct callback_state {
  struct out_buffer out_buffer;
  struct row_buffer *row_buffer_ptr;
  ErlNifEnv* env;
};

ErlNifResourceType* state_type;

static void add_value(void *s, int size, struct callback_state* cb_state_ptr) {
  struct row_buffer row_buffer = (*(*cb_state_ptr).row_buffer_ptr);
  ErlNifBinary *store = row_buffer.cols[row_buffer.col_n];
  enif_alloc_binary(size, store);
  (*store).size = size;
  memcpy((*store).data, s, size);
  row_buffer.col_n++;
}

static void add_row(struct callback_state* cb_state_ptr) {
  struct out_buffer out_buffer = (*cb_state_ptr).out_buffer;
  struct row_buffer row_buffer = (*(*cb_state_ptr).row_buffer_ptr);
  ErlNifEnv* env_ptr = (*cb_state_ptr).env;
  ERL_NIF_TERM cols[row_buffer.col_n];
  int i;
  int col_n = row_buffer.col_n;
  for (i = 0; i < col_n; i++) {
    cols[i] = enif_make_binary(env_ptr,
                               row_buffer.cols[i]);
  }
  row_buffer.col_n = 0;
  out_buffer.rows[out_buffer.row_n] = enif_make_list_from_array(env_ptr,
                                                                cols,
                                                                col_n);
  out_buffer.row_n++;
}

void column_callback (void *s, size_t size, void *cb_state_void_ptr) {
  printf("%.*s\t", (int) size, s);
  add_value(s,
            size,
            (struct callback_state*) cb_state_void_ptr);
}

void row_callback (int c, void *cb_state_void_ptr) {
  printf("\tNL\r\n");
  add_row((struct callback_state*) cb_state_void_ptr);
}

static ERL_NIF_TERM make_output(struct callback_state *cb_state_ptr) {
  ERL_NIF_TERM ret;
  ErlNifEnv* env_ptr = (*cb_state_ptr).env;
  struct out_buffer out_buffer = (*cb_state_ptr).out_buffer;
  ret = enif_make_list_from_array(env_ptr, out_buffer.rows, out_buffer.row_n);
  out_buffer.row_n = 0;
  return ret;
}

static struct state* init_state() {
  struct state* state_ptr = enif_alloc_resource(state_type,
                                                sizeof(struct state));
  (*state_ptr).row_buffer.col_n = 0;
  return state_ptr;
}

static void init_callback_state(struct callback_state *cb_state_ptr, ErlNifEnv* env_ptr, struct state *state_ptr) {
  (*cb_state_ptr).env = env_ptr;
  (*cb_state_ptr).out_buffer.row_n = 0;
  (*cb_state_ptr).row_buffer_ptr = &((*state_ptr).row_buffer);
}

static ERL_NIF_TERM init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM ret;
  struct state* state_ptr = init_state();
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
  struct callback_state cb_state;

  if (argc != 1) {
    return enif_make_badarg(env);
  }
  if (!enif_get_resource(env, argv[0], state_type, (void**) &state_ptr)) {
    return enif_make_badarg(env);
  }
  parser_ptr = &((*state_ptr).parser);

  init_callback_state(&cb_state, env, state_ptr);
  csv_fini(parser_ptr, column_callback, row_callback, &cb_state);
  return make_output(&cb_state);
}

static ERL_NIF_TERM parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary csv;
  struct state *state_ptr;
  struct csv_parser *parser_ptr;
  struct callback_state cb_state;

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

  printf("before init callback state\r\n");
  init_callback_state(&cb_state, env, state_ptr);
  printf("before parse\r\n");
  if (csv_parse(parser_ptr, csv.data, (size_t) csv.size,
                column_callback, row_callback, &cb_state) != csv.size) {
    return enif_make_string(env,
                            csv_strerror(csv_error(parser_ptr)),
                            ERL_NIF_LATIN1);
  }
  printf("before make output\r\n");
  return make_output(&cb_state);
}

static ErlNifFunc nif_funcs[] =
{
  {"init", 0, init},
  {"close", 1, close},
  {"parse", 2, parse},
};


void free_row_buffer(struct row_buffer buf) {
  int i;
  for (i = 0; i < buf.col_n; i++) {
    enif_release_binary(buf.cols[i]);
  }
}

void state_dtor(ErlNifEnv* env, void* obj_ptr)
{
  struct state* state_ptr = (struct state*) obj_ptr;
  struct csv_parser *parser_ptr = &((*state_ptr).parser);
  struct row_buffer row_buffer = (*state_ptr).row_buffer;
  printf("dtor!!\r\n");
  csv_free(parser_ptr);
  free_row_buffer(row_buffer);
  printf("done\r\n");
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
