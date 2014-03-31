#ifndef __EXPRESSION_H__
#define __EXPRESSION_H__

#include <string>
#include <vector>
#include <list>
#include <stdexcept>
#include <iostream>

enum IdentifierType {
    MODULE,
    IMPORT,
    FUNCTION,
    CLASS,
    INTERFACE,
    VARIABLE,
    STRUCT,
    UNION,
    LIST,
    CONSTRUCTOR,
    DESTRUCTOR,
    PRIMITIVE_TYPE,
    TYPE,
    DECL,
    OTHER
};

class Lines {
public:
    Lines(int first = -1, int last = -1) : m_first(first), m_last(last) {}
    bool isNull() const { return m_first<0 || m_last <0; }
    int first() const { return m_first; }
    int last() const { return m_last; }
private:
    int m_first;
    int m_last;
    friend class Node;
};

class PrimitiveTypeNode;
class ModuleNode;
class ImportNode;
class ClassNode;
class ConstructorNode;
class DestructorNode;
class NodeList;
class IdentifierNode;
class FunctionNode;
class VariableNode;


class AstVisitor
{
public:
    virtual void visit(PrimitiveTypeNode&) = 0;
    virtual void visit(ModuleNode&) = 0;
    virtual void visit(ImportNode&) = 0;
    virtual void visit(ClassNode&) = 0;
    virtual void visit(ConstructorNode&) = 0;
    virtual void visit(DestructorNode&) = 0;
    virtual void visit(IdentifierNode&) = 0;
    virtual void visit(NodeList&) = 0;
    virtual void visit(FunctionNode&) = 0;
    virtual void visit(VariableNode&) = 0;
    virtual ~AstVisitor() {}
};

class ThrowVisitor : public AstVisitor
{
    virtual void visit(PrimitiveTypeNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(ModuleNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(ImportNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(ClassNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(ConstructorNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(DestructorNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(IdentifierNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(NodeList&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(FunctionNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }

    virtual void visit(VariableNode&)
    {
        throw std::runtime_error("List visitor can visit only lists");
    }
};

class Node;

class DebugPrintVisitor : public AstVisitor
{
public:
    explicit DebugPrintVisitor(std::ostream& os = std::cout, int step = 1, char indentChar = ' ')
        : depth(0), step(step), indentChar(indentChar), os(os) {}
    virtual void visit(PrimitiveTypeNode&);
    virtual void visit(ModuleNode&);
    virtual void visit(ImportNode&);
    virtual void visit(ClassNode&);
    virtual void visit(ConstructorNode&);
    virtual void visit(DestructorNode&);
    virtual void visit(IdentifierNode&);
    virtual void visit(NodeList&);
    virtual void visit(FunctionNode&);
    virtual void visit(VariableNode&);

    void generateTree(Node &node);

private:
    int depth;
    int step;
    char indentChar;
    std::ostream& os;
};

class AstVisitable
{
public:
    virtual void accept(AstVisitor&) = 0;
    virtual ~AstVisitable() {}
};

#define MAKE_VISITABLE virtual void accept(AstVisitor& visitor) { visitor.visit(*this); }

class Node : public AstVisitable
{
public:
    Node(IdentifierType type, const std::string& identifier, Node* parent = 0, const std::string& hint = std::string())
        : m_type(type), m_identifier(identifier), m_parent(parent) {}

    virtual ~Node() {}

    Node* parent() {
        return m_parent;
    }
    typedef std::list<Node*> ChildsList;

    void setPosition(int first, int last);

    void setPosition(const Lines& lines);

    const Lines& getPosition() const {
        return m_pos;
    }

    virtual Node* clone() = 0;

    virtual ChildsList& childs() {
        return m_childs;
    }

    virtual const ChildsList& childs() const {
        return m_childs;
    }

    virtual void addChild(Node* child) {
        if (!child) return;
        child->m_parent = this;
        child->addToParent(this);
    }

    virtual const std::string& name() const {
        return m_identifier;
    }

    virtual void setName(const std::string& name) {
        m_identifier = name;
    }

protected:

    virtual void addToParent(Node* parent) {
        parent->m_childs.push_back(this);
    }

private:

    IdentifierType m_type;
    std::string m_identifier;
    Node* m_parent;
    Lines m_pos;

    ChildsList m_childs;
};

class NodeList : public Node
{
public:
    MAKE_VISITABLE

    explicit NodeList(const std::string& name = std::string()) : Node(LIST, name) {}

    Node* clone() {
        return new NodeList(*this);
    }

private:
    virtual void addToParent(Node* parent) {
        for (ChildsList::reverse_iterator it = childs().rbegin(); it != childs().rend(); ++it) {
            parent->addChild(*it);
        }
        delete this;
    }
};

class WithDefinitionNode : public Node
{
public:
    WithDefinitionNode(IdentifierType type, const std::string& identifier, Node* parent = 0, const std::string& hint = std::string())
        : Node(type, identifier, parent, hint) {}
    void setDefinitionPosition(const Lines& lines);
    void setDefinitionPosition(int first, int last);
    const Lines& getDefinitionPosition() const;
private:
    Lines m_definition_pos;
};

class ModuleNode: public Node
{
public:
    MAKE_VISITABLE
    ModuleNode(const std::string& name = std::string()) : Node(MODULE, name) {}
    virtual Node* clone() {
        return new ModuleNode(*this);
    }
};

class ImportNode: public Node
{
public:
    MAKE_VISITABLE
    ImportNode(const std::string& name = std::string()) : Node(IMPORT, name) {}
    virtual Node* clone() {
        return new ImportNode(*this);
    }
};

class ClassNode: public Node
{
public:
    MAKE_VISITABLE
    ClassNode(const std::string& name = std::string()) : Node(CLASS, name) {}
    virtual Node* clone() {
        return new ClassNode(*this);
    }
};


class FunctionNode : public WithDefinitionNode
{
public:
    MAKE_VISITABLE
    FunctionNode(const std::string& name, Node* list);
    void setDefinition(Node* scopeList);
    void setReturnType(Node* type);
    virtual Node* clone();
private:
    NodeList parameters;
    Node* returnType;
};

class VariableNode : public Node
{
public:
    MAKE_VISITABLE
    VariableNode(const std::string& name, Node* type);
    void setType(Node* type);
    virtual VariableNode* clone();
private:
    Node* m_type;
};

class ConstructorNode: public FunctionNode
{
public:
    MAKE_VISITABLE
    ConstructorNode(Node* list) : FunctionNode("this", list) {}

    virtual Node* clone() {
        return new ConstructorNode(*this);
    }
};

class DestructorNode : public Node
{
public:
    MAKE_VISITABLE
    DestructorNode() : Node(DESTRUCTOR, "") {}
    virtual Node* clone() {
        return new DestructorNode(*this);
    }
private:
    virtual void addToParent(Node* parent) {
        setName(parent->name());
        Node::addToParent(parent);
    }

};

class PrimitiveTypeNode : public Node
{
public:
    MAKE_VISITABLE
    enum Type {
        BOOL,
        BYTE,
        UBYTE,
        SHORT,
        USHORT,
        INT,
        UINT,
        LONG,
        ULONG,
        CHAR,
        WCHAR,
        DCHAR,
        FLOAT,
        DOUBLE,
        REAL,
        IFLOAT,
        IDOUBLE,
        IREAL,
        CFLOAT,
        CDOUBLE,
        CREAL,
        VOID
    };

    PrimitiveTypeNode(Type type) : Node(PRIMITIVE_TYPE, getPrimitiveTypeName(type)), m_type(type) {}

    virtual Node* clone() {
        return new PrimitiveTypeNode(*this);
    }

    Type getPrimitiveType() const
    {
        return m_type;
    }

    static std::string getPrimitiveTypeName(Type type);

private:
    Type m_type;

};

class BasicTypeVisitor : public ThrowVisitor {
public:
    BasicTypeVisitor(Node* type) : m_type(type) {}

    virtual void visit(NodeList& list)
    {
        for (auto& node : list.childs())
        {
            node->accept(*this);
        }
    }

    virtual void visit(FunctionNode& func)
    {
        func.setReturnType(m_type);
    }

    virtual void visit(VariableNode& var)
    {
        var.setType(m_type);
    }

private:
    Node* m_type;
};


class IdentifierNode : public Node
{
public:
    MAKE_VISITABLE
    IdentifierNode(const std::string& name) : Node(VARIABLE, name) {}
    virtual void addChild(Node* child) {
        throw std::runtime_error("Identifier has no childs");
    }

    virtual Node* clone() {
        return new IdentifierNode(*this);
    }

};

std::ostream& operator <<(std::ostream& os, Node& node);

#endif // __EXPRESSION_H__
