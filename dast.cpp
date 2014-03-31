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

std::ostream& operator <<(std::ostream& os, Node& node)
{
    DebugPrintVisitor v(os, 2, '-');
    v.generateTree(node);
    return os;
}

void Node::setPosition(int first, int last)
{
    m_pos.m_first = first;
    m_pos.m_last = last;
}

void Node::setPosition(const Lines &lines)
{
    this->m_pos = lines;
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


void WithDefinitionNode::setDefinitionPosition(const Lines &lines)
{
    this->m_definition_pos = lines;
}

void WithDefinitionNode::setDefinitionPosition(int first, int last)
{
    setDefinitionPosition(Lines(first, last));
}

const Lines &WithDefinitionNode::getDefinitionPosition() const
{
    return this->m_definition_pos;
}

class SetDefinitionVisitor : public ThrowVisitor
{
public:
    SetDefinitionVisitor(WithDefinitionNode* node) : node(node) {}
    void visit(NodeList& list)
    {
        if (!node) throw std::runtime_error("Invalid node with definition");
        node->addChild(&list);
        node->setDefinitionPosition(list.getPosition());
    }
private:
    WithDefinitionNode* node;
};

FunctionNode::FunctionNode(const std::string &name, Node *list)
    : WithDefinitionNode(FUNCTION, name)
{
    if (!list) return;
    if (list->childs().empty()) return;
    for (ChildsList::iterator it = list->childs().begin(); it != list->childs().end(); ++it) {
        parameters.addChild(*it);
    }
    addChild(list);
}

void FunctionNode::setDefinition(Node *scopeList)
{

}

void FunctionNode::setReturnType(Node *type)
{
    this->returnType = type;
}

Node *FunctionNode::clone()
{
    return new FunctionNode(*this);
}


VariableNode::VariableNode(const std::string &name, Node *type)
    : Node(VARIABLE, name), m_type(type)
{

}

void VariableNode::setType(Node *type)
{
    m_type = type;
}

VariableNode *VariableNode::clone()
{
    return new VariableNode(*this);
}

static std::string to_string(const Lines& lines)
{
    return std::string("(" + std::to_string(lines.first()) + "," + std::to_string(lines.last()) + ")");
}

static std::string getPositionString(const Node& node)
{
    return " decl" + to_string(node.getPosition());
}

static std::string getPositionString(const WithDefinitionNode& node)
{
    return getPositionString(*static_cast<const Node*>(&node)) + " def" + to_string(node.getDefinitionPosition());
}

void DebugPrintVisitor::visit(PrimitiveTypeNode&)
{
    throw std::runtime_error("PrimitiveTypeNode in AST");
}

void DebugPrintVisitor::visit(ModuleNode& module)
{
    os << "[Module]" << module.name() << getPositionString(module) << std::endl;
}

void DebugPrintVisitor::visit(ImportNode& import)
{
    os << "[Import]" << import.name() << getPositionString(import) << std::endl;
}

void DebugPrintVisitor::visit(ClassNode& cl)
{
    os << "[Class]" << cl.name() << getPositionString(cl) << std::endl;
}

void DebugPrintVisitor::visit(ConstructorNode& constructor)
{
    os << "[Constructor]" << getPositionString(constructor) << std::endl;
}

void DebugPrintVisitor::visit(DestructorNode& destructor)
{
    os << "[Destructor]" << getPositionString(destructor) << std::endl;
}

void DebugPrintVisitor::visit(IdentifierNode& id)
{
    os << "[Identifier]" << id.name() << std::endl;
}

void DebugPrintVisitor::visit(NodeList& list)
{
    os << "[List]" << list.name() << getPositionString(list) << std::endl;
}

void DebugPrintVisitor::visit(FunctionNode& func)
{
    os << "[Function]" << func.name() << getPositionString(func) << std::endl;
}

void DebugPrintVisitor::visit(VariableNode& var)
{
    os << "[Variable]" << var.name() << getPositionString(var) << std::endl;
}

void DebugPrintVisitor::generateTree(Node &node)
{
    for (int i = 0; i < step * depth; ++i)
    {
        os << indentChar;
    }
    node.accept(*this);
    ++depth;
    for (auto& child : node.childs())
    {
        generateTree(*child);
    }
    --depth;
}
