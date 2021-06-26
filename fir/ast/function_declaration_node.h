#ifndef __FIR_AST_FUNCTION_DECLARATION_H__
#define __FIR_AST_FUNCTION_DECLARATION_H__

#include <string>
#include <cdk/ast/typed_node.h>
#include <cdk/ast/sequence_node.h>

namespace fir {

  //!
  //! Class for describing function declarations.
  //! <pre>
  //! declaration: type qualifier id '(' args ')'
  //!            {
  //!              new og::function::Declaration(LINE, $1, $2, $3, $5);
  //!            }
  //! </pre>
  //!
  class function_declaration_node: public cdk::typed_node {
    int _qualifier;
    std::string _identifier;
    cdk::sequence_node *_arguments;
    /*cdk::integer_node *_returnValue;*/
    fir::body_node *_body;

  public:
    function_declaration_node(int lineno, std::shared_ptr<cdk::basic_type> funType, int qualifier, const std::string &identifier, cdk::sequence_node *arguments,
                               /*cdk::integer_node *returnValue,*/ fir::body_node *body) :
        cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _arguments(arguments), /*_returnValue(returnValue),*/ _body(body) {
      type(funType);
    }

  public:
    int qualifier() {
      return _qualifier;
    }
    const std::string& identifier() const {
      return _identifier;
    }
    cdk::typed_node* argument(size_t ax) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(ax));
    }
    cdk::sequence_node* arguments() {
      return _arguments;
    }
    /*cdk::integer_node* returnValue() {
      return _returnValue;
    }*/
    fir::body_node* body() {
      return _body;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_declaration_node(this, level);
    }

  };

} // fir

#endif
