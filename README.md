CSV - High performance CSV decoder for Erlang
=============================================

Erlang CSV decoder (no encoder) implemented as a NIF using libcsv.

Notes
=====
- All values are returned as Erlang strings with one item per decoded byte. E.g. a 2 byte UTF-8 character will be returned as a list with 2 elements, which can be encoded into a valid UTF-8 binary by `erlang:list_to_binary/1`.
- The `reload`, `upgrade`, `unload` callbacks are not implemented which will potentially cause crashes when hot code upgrading to a future version.

Todo
====
 - Only return the wanted column values to Erlang instead of all column values in the CSV.
 - Allocate less memory for `out_buffer` by only storing the wanted columns.

License
=======
The MIT License (MIT). See LICENSE for details.
