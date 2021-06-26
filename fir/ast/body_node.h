#ifndef __FIR_AST_BODY_H__
#define __FIR_AST_BODY_H__


namespace fir {

  class body_node: public cdk::basic_node {
    fir::block_node *_prologue, *_block, *_epilogue;

  public:
    body_node(int lineno, fir::block_node *prologue = nullptr, fir::block_node *block = nullptr, fir::block_node *epilogue = nullptr) :
        cdk::basic_node(lineno), _prologue(prologue), _block(block), _epilogue(epilogue) {
    }

  public:
    fir::block_node* prologue() {
      return _prologue;
    }
    fir::block_node* block() {
      return _block;
    }
    fir::block_node* epilogue() {
      return _epilogue;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_body_node(this, level);
    }

  };

} // fir

#endif
