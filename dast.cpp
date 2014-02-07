/*
 * Expression.c
 * Implementation of functions used to build the syntax tree.
 */

#include "dast.h"
#include "memory"


void DeclarationTypedList::addToParent(Node *parent) {
    for (ChildsList::iterator it = childs().begin(); it != childs().end(); ++it) {
        std::auto_ptr<Node> ctype(m_type->clone());
        std::auto_ptr<DeclarationNode> decl(new DeclarationNode(ctype.get(), *it));
        parent->addChild(decl.get());
        decl.release();
        ctype.release();
    }
    delete m_type;
    delete this;
}

