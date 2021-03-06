/*
 * List of all the SUBRs with Keywords
 * Bruno Haible 1990-2004
 * Sam Steingold 1998-2008
 */

v(7, (kw(adjustable),kw(element_type),kw(initial_element),
      kw(initial_contents),kw(fill_pointer),
      kw(displaced_to),kw(displaced_index_offset)) )
s(make_array)
v(6, (kw(element_type),kw(initial_element),
      kw(initial_contents),kw(fill_pointer),
      kw(displaced_to),kw(displaced_index_offset)) )
s(adjust_array)
v(4, (kw(start1),kw(end1),kw(start2),kw(end2)) )
s(string_eq)
s(cs_string_eq)
s(string_noteq)
s(cs_string_noteq)
s(string_less)
s(cs_string_less)
s(string_greater)
s(cs_string_greater)
s(string_ltequal)
s(cs_string_ltequal)
s(string_gtequal)
s(cs_string_gtequal)
s(string_equal)
s(string_not_equal)
s(string_lessp)
s(string_greaterp)
s(string_not_greaterp)
s(string_not_lessp)
s(search_string_eq)
s(search_string_equal)
s(replace)
v(1, (kw(initial_element)) )
s(make_list)
v(2, (kw(initial_element),kw(element_type)) )
s(make_string)
v(2, (kw(start),kw(end)) )
s(string_width)
s(nstring_upcase)
s(string_upcase)
s(nstring_downcase)
s(string_downcase)
s(nstring_capitalize)
s(string_capitalize)
s(nstring_invertcase)
s(string_invertcase)
s(write_string)
s(write_line)
s(coerced_subseq)
s(fill)
s(read_char_sequence)
s(write_char_sequence)
s(convert_string_from_bytes)
s(convert_string_to_bytes)
v(4, (kw(start),kw(end),kw(no_hang),kw(interactive)) )
s(read_byte_sequence)
s(write_byte_sequence)
v(5, (kw(charset),kw(line_terminator),kw(input_error_action),
      kw(output_error_action),kw(if_does_not_exist)) )
s(make_encoding)
v(9, (kw(initial_contents),kw(key_type),kw(value_type),
      kw(warn_if_needs_rehash_after_gc),kw(weak),
      kw(test),kw(size),kw(rehash_size),kw(rehash_threshold)) )
s(make_hash_table)
v(3, (kw(preserve_whitespace),kw(start),kw(end)) )
s(read_from_string)
v(4, (kw(start),kw(end),kw(radix),kw(junk_allowed)) )
s(parse_integer)
v(17, (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),
       kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(readably),
       kw(lines),kw(miser_width),kw(pprint_dispatch),
       kw(right_margin),kw(stream)))
s(write)
v(16, (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),
       kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(readably),
       kw(lines),kw(miser_width),kw(pprint_dispatch),kw(right_margin)))
s(write_to_string)
v(2, (kw(type),kw(identity)) )
s(write_unreadable)
v(2, (kw(test),kw(test_not)) )
s(tree_equal)
v(3, (kw(test),kw(test_not),kw(key)) )
s(subst)
s(nsubst)
s(sublis)
s(nsublis)
s(member)
s(adjoin)
s(assoc)
s(rassoc)
v(1, (kw(key)) )
s(subst_if)
s(subst_if_not)
s(nsubst_if)
s(nsubst_if_not)
s(member_if)
s(member_if_not)
s(assoc_if)
s(assoc_if_not)
s(rassoc_if)
s(rassoc_if_not)
s(merge)
v(4, (kw(nicknames),kw(use),kw(case_sensitive),kw(case_inverted)) )
s(make_package)
s(cs_make_package)
s(pin_package)
v(3, (kw(start),kw(end),kw(junk_allowed)) )
s(parse_namestring)
v(1, (kw(case)) )
s(pathnamehost)
s(pathnamedevice)
s(pathnamedirectory)
s(pathnamename)
s(pathnametype)
#ifdef LOGICAL_PATHNAMES
v(1, (kw(absolute)))
s(translate_logical_pathname)
#endif
v(1, (kw(wild)) )
s(merge_pathnames)
v(8, (kw(defaults),kw(case),kw(host),kw(device),kw(directory),kw(name),kw(type),kw(version)) )
s(make_pathname)
#ifdef LOGICAL_PATHNAMES
s(make_logical_pathname)
#endif
v(3, (kw(all),kw(merge),kw(absolute)) )
s(translate_pathname)
v(6, (kw(direction),kw(element_type),kw(if_exists),kw(if_does_not_exist),kw(external_format),kw(buffered)) )
s(open)
v(3, (kw(if_does_not_exist),kw(circle),kw(full)) )
s(directory)
v(1, (kw(verbose)))
s(ensure_directories_exist)
#if defined(UNIX) || defined(WIN32_NATIVE)
v(9, (kw(element_type),kw(external_format),kw(buffered),kw(arguments),kw(wait),kw(input),kw(output),kw(error),kw(priority)))
s(launch)
#endif
v(2, (kw(type),kw(initial_contents)) )
s(make_weak_alist)
v(3, (kw(test),kw(test_not),kw(key)) )
s(weak_alist_assoc)
s(weak_alist_rassoc)
v(2, (kw(test),kw(test_not)) )
s(weak_alist_value)
s(set_weak_alist_value)
v(2, (kw(initial_element),kw(update)) )
s(make_sequence)
v(5, (kw(from_end),kw(start),kw(end),kw(key),kw(initial_value)) )
s(reduce)
v(7, (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
s(remove)
s(delete)
s(substitute)
s(nsubstitute)
v(5, (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
s(remove_if)
s(remove_if_not)
s(delete_if)
s(delete_if_not)
s(substitute_if)
s(substitute_if_not)
s(nsubstitute_if)
s(nsubstitute_if_not)
v(6, (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
s(remove_duplicates)
s(delete_duplicates)
s(find)
s(position)
s(count)
v(4, (kw(from_end),kw(start),kw(end),kw(key)) )
s(find_if)
s(find_if_not)
s(position_if)
s(position_if_not)
s(count_if)
s(count_if_not)
v(8, (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),
      kw(key),kw(test),kw(test_not)) )
s(mismatch)
s(search)
v(3, (kw(key),kw(start),kw(end)) )
s(sort)
s(stable_sort)
v(2, (kw(element_type),kw(line_position)) )
s(make_string_output_stream)
#ifdef PIPES
v(3, (kw(element_type),kw(external_format),kw(buffered)) )
s(make_pipe_input_stream)
s(make_pipe_output_stream)
#ifdef PIPES2
s(make_pipe_io_stream)
#endif
#endif
#ifdef SOCKET_STREAMS
v(4, (kw(element_type),kw(external_format),kw(buffered),kw(timeout)) )
s(socket_accept)
s(socket_connect)
v(4, (kw(direction),kw(element_type),kw(external_format),kw(buffered)) )
s(make_stream)
v(2, (kw(backlog),kw(interface)) )
s(socket_server)
#endif
v(1, (kw(abort)) )
s(built_in_stream_close)
#ifdef DYNAMIC_FFI
v(1, (kw(name)))
s(foreign_function)
s(foreign_variable)
v(3, (kw(initial_contents),kw(count),kw(read_only)))
s(foreign_allocate)
v(1, (kw(full)))
s(foreign_free)
#endif
#ifdef MULTITHREAD
v(1, (kw(name)))
s(make_thread)
#endif
v(7, (kw(name),kw(code),kw(constants),kw(seclass),kw(lambda_list),kw(documentation),kw(jitc_p)) )
s(make_closure)
