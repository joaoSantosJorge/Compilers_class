#ifndef __FIR_AST_IDENTITY_NODE_H__
#define __FIR_AST_IDENTITY_NODE_H__

#include <cdk/ast/expression_node.h>

namespace fir {

  /**
   * Class for describing identity nodes.
   */
  class identity_node: public cdk::expression_node {
    cdk::expression_node *_identity;

  public:
    inline identity_node(int lineno, cdk::expression_node *identity) :
        cdk::expression_node(lineno), _identity(identity) {
    }

  public:
    inline cdk::expression_node *identity() {
      return _identity;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_identity_node(this, level);
    }

  };

} // fir

#endif
