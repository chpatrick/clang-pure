/*
Copyright 2014 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

typedef void (*haskell_visitor)(CXCursor*);

// Traverse children using a haskell_visitor passed in as client_data.
// The visitor gets a copy of the cursor on the heap and is responsible
// for freeing it.
static enum CXChildVisitResult visit_haskell(CXCursor cursor, CXCursor parent, CXClientData client_data) {
  CXCursor *heapCursor = malloc(sizeof(CXCursor));
  *heapCursor = cursor;
  ((haskell_visitor) client_data)(heapCursor);
  return CXChildVisit_Continue;
};

// Macro that makes a copy of the result of a given expression on the heap and returns it.
#define ALLOC(__ALLOC_TYPE__, __ALLOC_EXPR__) {\
  __ALLOC_TYPE__ *__alloc_ptr__ = malloc(sizeof(__ALLOC_TYPE__));\
  *__alloc_ptr__ = (__ALLOC_EXPR__);\
  return __alloc_ptr__;\
  }
