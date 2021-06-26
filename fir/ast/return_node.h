#ifndef __FIR_AST_RETURN_H__
#define __FIR_AST_RETURN_H__

#include <cdk/ast/basic_node.h>

namespace fir {

  class return_node: public cdk::basic_node {
    //maybe does not have a return value. the return value may be stored in the funtion
    cdk::basic_node *_retval;

  public:
    return_node(int lineno, cdk::basic_node *retval = nullptr) :
        cdk::basic_node(lineno), _retval(retval) {
    }

  public:
    cdk::basic_node* retval() {
      return _retval;
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_return_node(this, level);
    }

  };

} // fir

#endif
