/*
 * Expression.c
 * Implementation of functions used to build the syntax tree.
 */

#include "dast.h"
#include "memory"


static void generateTree(std::ostream& os, const Node& node, int depth = 0) {
    for (int i = 0; i < depth; ++i) os << '-';
    os << node.ToDebugString() << '\n';
    for (Node::ChildsList::const_iterator it = node.childs().begin(); it != node.childs().end(); ++it)
    {
        generateTree(os, **it, depth + 1);
    }
}

std::ostream& operator <<(std::ostream& os, const Node& node)
{
    generateTree(os, node);
    return os;
}

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



std::string Node::ToDebugString() const
{
    const char* typeStr = "UnknownType";
    switch (m_type)
    {
    case MODULE : typeStr = "Module"; break;
    case IMPORT : typeStr = "Import"; break;
    case FUNCTION : typeStr = "Function"; break;
    case CLASS: typeStr = "Class"; break;
    case INTERFACE : typeStr = "Interface"; break;
    case VARIABLE : typeStr = "Variable"; break;
    case STRUCT : typeStr = "Struct"; break;
    case UNION : typeStr = "Union"; break;
    case LIST : typeStr = "List"; break;
    case CONSTRUCTOR: typeStr = "Constructor"; break;
    case DESTRUCTOR: typeStr = "Destructor"; break;
    case PRIMITIVE_TYPE : typeStr = "PrimitiveType"; break;
    case TYPE : typeStr = "Type"; break;
    case OTHER : typeStr = "Other"; break;
    }
    std::string res = "[";
    res += typeStr;
    res += "]";
    res += m_identifier;
    return res;
}
