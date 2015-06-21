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
