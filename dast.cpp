/*
 * Expression.c
 * Implementation of functions used to build the syntax tree.
 */

#include "dast.h"

#include <memory>
#include <cstdio>

static std::string toString(int a)
{
    char buff[1024];
    snprintf(buff, 1024, "%d", a);
    return buff;
}

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
    if (!pos.isNull()) {
        res+="(";
        res+=toString(pos.first());
        res+="-";
        res+=toString(pos.last());
        res+=")";
    }
    return res;
}


std::string PrimitiveTypeNode::getPrimitiveTypeName(PrimitiveTypeNode::Type type)
{
    switch (type)
    {
    case BOOL : return "bool";
    case BYTE : return "byte";
    case UBYTE : return "ubyte";
    case SHORT : return "short";
    case USHORT : return "ushort";
    case INT : return "int";
    case UINT : return "uint";
    case LONG : return "long";
    case ULONG : return "ulong";
    case CHAR : return "char";
    case WCHAR : return "wchar";
    case DCHAR : return "dchar";
    case FLOAT : return "float";
    case DOUBLE : return "double";
    case REAL : return "real";
    case IFLOAT : return "ifloat";
    case IDOUBLE : return "idouble";
    case IREAL : return "ireal";
    case CFLOAT : return "cfloat";
    case CDOUBLE : return "cdouble";
    case CREAL : return "creal";
    case VOID : return "void";
    }
    return "unknown";
}
