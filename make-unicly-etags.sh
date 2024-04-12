#!/bin/sh
#
#
###  ==============================
# :FILE-CREATED <Timestamp: #{2024-04-10T12:16:17-04:00Z}#{24153} - by MON KEY>
# :FILE unicly/make-unicly-etags.sh
#
# quickly find all the lisp files here:
# shell> find . -name "*.lisp" -print
#
#  Add the current directory to the tail of Emacs' `tags-table-list'
# (add-to-list 'tags-table-list default-directory t)
#
# Make sure to tell customize or it'll wind up bitching:
# (custom-note-var-changed 'tags-table-list)
# 
#
###  ==============================

etags ./unicly-bit-vectors.lisp \
./unicly-bridge.lisp \
./unicly-byte-arrays.lisp \
./unicly-class.lisp \
./unicly-compat.lisp \
./unicly-conditions.lisp \
./unicly-deprecated.lisp \
./unicly-docs.lisp \
./unicly-extend.lisp \
./unicly-hash-table.lisp \
./unicly-integers.lisp \
./unicly-io.lisp \
./unicly-macros.lisp \
./unicly-null-check.lisp \
./unicly-specials.lisp \
./unicly-string-uuid.lisp \
./unicly-types.lisp \
./unicly-utils.lisp \
./unicly-uuid-version.lisp \
./unicly.lisp \
--language=lisp

###  ==============================
### EOF
