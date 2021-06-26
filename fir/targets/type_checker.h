#ifndef __FIR_TARGETS_TYPE_CHECKER_H__
#define __FIR_TARGETS_TYPE_CHECKER_H__

#include "targets/basic_ast_visitor.h"

namespace fir {

  /**
   * Print nodes as XML elements to the output stream.
   */
  class type_checker: public basic_ast_visitor {
    cdk::symbol_table<fir::symbol> &_symtab;
    std::shared_ptr<fir::symbol> _function;
    basic_ast_visitor *_parent;
    std::shared_ptr<cdk::basic_type> _inBlockReturnType = nullptr;

  public:
    type_checker(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<fir::symbol> &symtab, basic_ast_visitor *parent,
          std::shared_ptr<fir::symbol> func) :
        basic_ast_visitor(compiler), _symtab(symtab), _function(func), _parent(parent) {
    }

  public:
    ~type_checker() {
      os().flush();
    }

  protected:
    template<typename T>
    void process_literal(cdk::literal_node<T> *const node, int lvl) {
    }


  protected:
    void do_IntOnlyExpression(cdk::binary_operation_node * const node, int lvl);
    void do_PIDExpression(cdk::binary_operation_node * const node, int lvl);
    void do_IDExpression(cdk::binary_operation_node * const node, int lvl);
  protected:
    void do_ScalarLogicalExpression(cdk::binary_operation_node * const node, int lvl);
    void do_BooleanLogicalExpression(cdk::binary_operation_node * const node, int lvl);
    void do_GeneralLogicalExpression(cdk::binary_operation_node * const node, int lvl);

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include "ast/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end

  };


//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

#define CHECK_TYPES(compiler, symtab, function, node) { \
    try { \
      fir::type_checker checker(compiler, symtab, function, this); \
      (node)->accept(&checker, 0); \
    } \
    catch (const std::string &problem) { \
      std::cerr << (node)->lineno() << ": " << problem << std::endl; \
      return; \
    } \
  }

#define ASSERT_SAFE CHECK_TYPES(_compiler, _symtab, _function, node)

} // fir

#endif
