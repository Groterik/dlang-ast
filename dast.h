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

class Node
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
    ModuleNode(const std::string& name = std::string()) : Node(MODULE, name) {}
    virtual Node* clone() {
        return new ModuleNode(*this);
    }
};

class ImportNode: public Node
{
public:
    ImportNode(const std::string& name = std::string()) : Node(IMPORT, name) {}
    virtual Node* clone() {
        return new ImportNode(*this);
    }
};

class ClassNode: public Node
{
public:
    ClassNode(const std::string& name = std::string()) : Node(CLASS, name) {}
    virtual Node* clone() {
        return new ClassNode(*this);
    }
};

class DeclarationNode: public Node {
public:
    DeclarationNode(Node* type, Node* declarator)
        : Node(TYPE, type->name() + " " + (declarator?declarator->name():std::string())), m_type(type), m_declarator(declarator), value(0) {}
    void setValue(Node* value) {
        this->value = value;
    }

    virtual Node* clone() {
        return new DeclarationNode(*this);
    }

private:
    Node* m_type;
    Node* m_declarator;
    Node* value;
};

class ConstructorNode: public Node
{
public:
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

class DeclarationTypedList : public NodeList {
public:
    DeclarationTypedList(Node* type) : m_type(type) {}
    virtual Node* clone() {
        return new DeclarationTypedList(*this);
    }

    virtual void addChild(Node *child) {
        NodeList::addChild(child);
    }

private:
    virtual void addToParent(Node *parent);
    Node* m_type;
};


class IdentifierNode : public Node
{
public:
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
