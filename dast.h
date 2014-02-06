/*
 * Expression.h
 * Definition of the structure used to build the syntax tree.
 */
#ifndef __EXPRESSION_H__
#define __EXPRESSION_H__

#include <string>
#include <vector>
#include <list>
#include <stdexcept>

#include <iostream>

enum IdentifierType {
    MODULE,
    FUNCTION,
    CLASS,
    INTERFACE,
    VARIABLE,
    STRUCT,
    UNION,
    OTHER
};

class Expression;

class ExprList : std::list<Expression*>
{
public:
    typedef std::list<Expression*> Base;
    ExprList() {}

    using Base::const_iterator;
    using Base::iterator;
    using Base::begin;
    using Base::end;
    using Base::push_back;

    using Base::push_front;
};

//typedef std::list<Expression*> ExprList;

class Expression
{
public:
    Expression(IdentifierType type, const std::string& identifier, Expression* parent = 0, const std::string& hint = std::string())
        : m_type(type), m_identifier(identifier), m_parent(parent) {}



    virtual ~Expression() {}

    Expression* parent() {
        return m_parent;
    }
    typedef ExprList ChildsList;

    virtual ChildsList childs() {
        return m_childs;
    }

    virtual void addChild(Expression* child) {
        m_childs.push_back(child);
    }

    virtual void addChild(const ExprList* child) {
        if (!child) return;
        for (ExprList::const_iterator it = child->begin(); it != child->end(); ++it) {
            m_childs.push_back(*it);
        }
    }

    virtual const std::string& name() const {
        return m_identifier;
    }

private:
    IdentifierType m_type;
    std::string m_identifier;
    Expression* m_parent;

    ChildsList m_childs;
};

class Module: public Expression
{
public:
    Module(const std::string& name = std::string()) : Expression(MODULE, name) {}
};

class Import: public Expression
{
public:
    Import(const std::string& name = std::string()) : Expression(MODULE, name) {}
};

class Identifier : public Expression
{
public:
    Identifier(const std::string& name) : Expression(VARIABLE, name) {}
    virtual void addChild(Expression* child) {
        throw std::runtime_error("Identifier has no childs");
    }

};

#endif // __EXPRESSION_H__
