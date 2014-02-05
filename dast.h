/*
 * Expression.h
 * Definition of the structure used to build the syntax tree.
 */
#ifndef __EXPRESSION_H__
#define __EXPRESSION_H__

#include <string>
#include <vector>
#include <stdexcept>

#include <iostream>

enum IdentifierType {
    MODULE,
    FUNCTION,
    CLASS,
    INTERFACE,
    VARIABLE,
    STRUCT,
    UNION
};

class Expression {
public:
    Expression(IdentifierType type, const std::string& identifier, Expression* parent = 0, const std::string& hint = std::string())
        : m_type(type), m_identifier(identifier), m_parent(parent) {}



    virtual ~Expression() {}

    Expression* parent() {
        return m_parent;
    }
    typedef std::vector<Expression*> ChildsVector;

    virtual ChildsVector childs() {
        return m_childs;
    }

    virtual void addChild(Expression* child) {
        m_childs.push_back(child);
    }

    virtual const std::string& name() const {
        return m_identifier;
    }

private:
    IdentifierType m_type;
    std::string m_identifier;
    Expression* m_parent;

    ChildsVector m_childs;
};

class Identifier : public Expression{
public:
    Identifier(const std::string& name) : Expression(VARIABLE, name) {}
    virtual void addChild(Expression *child) {
        throw std::runtime_error("Identifier has no childs");
    }

};

#endif // __EXPRESSION_H__
