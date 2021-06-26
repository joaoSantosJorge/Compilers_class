#ifndef __FIR_AST_LEAVE_H__
#define __FIR_AST_LEAVE_H__

#include <cdk/ast/basic_node.h>

namespace fir {

  class leave_node: public cdk::basic_node {
    int _level;

  public:
    leave_node(int lineno, int level = 1) :
        cdk::basic_node(lineno), _level(level) {
    }

  public:
    int level() const {
      return _level;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_leave_node(this, level);
    }

  };

} // og

#endif
