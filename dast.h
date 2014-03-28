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

class DeclarationNode;
class PrimitiveTypeNode;
class ModuleNode;
class ImportNode;
class ClassNode;
class ConstructorNode;
class DestructorNode;
class NodeList;
class IdentifierNode;
class FunctionNode;


class AstVisitor {
public:
    virtual void visit(DeclarationNode&) = 0;
    virtual void visit(PrimitiveTypeNode&) = 0;
    virtual void visit(ModuleNode&) = 0;
    virtual void visit(ImportNode&) = 0;
    virtual void visit(ClassNode&) = 0;
    virtual void visit(ConstructorNode&) = 0;
    virtual void visit(DestructorNode&) = 0;
    virtual void visit(IdentifierNode&) = 0;
    virtual void visit(NodeList&) = 0;
    virtual void visit(FunctionNode&) = 0;
    virtual ~AstVisitor() {}
};

class AstVisitable {
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

    void setPosition(int first, int last) {
        pos.m_first = first;
        pos.m_last = last;
    }

    const Lines& getPosition() const {
        return pos;
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

    virtual std::string ToDebugString() const;

protected:

    virtual void addToParent(Node* parent) {
        parent->m_childs.push_back(this);
    }

private:

    IdentifierType m_type;
    std::string m_identifier;
    Node* m_parent;
    Lines pos;

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

class DeclarationNode: public Node {
public:
    MAKE_VISITABLE
    DeclarationNode(Node* type, const std::string& identifier)
        : Node(DECL, identifier), m_type(type), value(0) {}
    void setValue(Node* value) {
        this->value = value;
    }

    void setType(Node* type) {
        this->m_type = type;
    }

    virtual Node* clone() {
        return new DeclarationNode(*this);
    }

    virtual std::string ToDebugString() const;

private:
    Node* m_type;
    Node* qualifiers;
    Node* value;
};


class FunctionNode : public Node
{
public:
    MAKE_VISITABLE
    FunctionNode(const std::string& name, Node* list) : Node(FUNCTION, name) {
        if (!list) return;
        if (list->childs().empty()) return;
        for (ChildsList::iterator it = list->childs().begin(); it != list->childs().end(); ++it) {
            parameters.addChild(*it);
        }
        addChild(list);
    }

    void setReturnType(Node* type)
    {
        this->returnType = type;
    }

    virtual Node* clone() {
        return new FunctionNode(*this);
    }

private:
    NodeList parameters;
    Node* returnType;
};

class ConstructorNode: public Node
{
public:
    MAKE_VISITABLE
    ConstructorNode(Node* list) : Node(CONSTRUCTOR, "") {
        if (!list) return;
        if (list->childs().empty()) return;
        for (ChildsList::iterator it = list->childs().begin(); it != list->childs().end(); ++it) {
            parameters.addChild(*it);
        }
        addChild(list);
    }

    virtual Node* clone() {
        return new ConstructorNode(*this);
    }

private:
    virtual void addToParent(Node* parent) {
        setName(parent->name());
        Node::addToParent(parent);
    }

    NodeList parameters;
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

private:
    static std::string getPrimitiveTypeName(Type type);
    Type m_type;

};

class BasicTypeVisitor : public AstVisitor {
public:
    virtual void visit(DeclarationNode& decl)
    {
        decl.setType(m_type);
    }

    virtual void visit(PrimitiveTypeNode&) {}
    virtual void visit(ModuleNode&) {}
    virtual void visit(ImportNode&) {}
    virtual void visit(ClassNode&) {}
    virtual void visit(ConstructorNode&) {}
    virtual void visit(DestructorNode&) {}
    virtual void visit(IdentifierNode&) {}
    virtual void visit(NodeList&) {}
    virtual void visit(FunctionNode& func)
    {
        func.setReturnType(m_type);
    }
};

class DeclarationTypedList : public NodeList, public AstVisitor {
public:


    DeclarationTypedList(Node* type) : m_type(type) {}
    virtual Node* clone() {
        return new DeclarationTypedList(*this);
    }

    virtual void addChild(Node* child);

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

std::ostream& operator <<(std::ostream& os, const Node& node);

#endif // __EXPRESSION_H__
