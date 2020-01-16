use super::ast::{
    callable::{NativeFunction, GLOBALS},
    expression::{ExprIdentifier, Expression},
    operator::{BinaryOperator, UnaryOperator},
    types::{Type, Typed},
    Literal, Locatable, Location, Statement, Token,
};
use super::error::{ParserError, TypeError};
use std::collections::HashMap;
use std::iter::Peekable;
use std::mem::{discriminant, swap};

type TypeScope = HashMap<String, Type>;

#[derive(Debug)]
enum CallableTypeSpecifier {
    Subroutine(Vec<Type>, Type),
    NativeFunction(NativeFunction),
}

impl CallableTypeSpecifier {
    fn validate(&self, arg_types: &Vec<Type>) -> Result<(), ParserError> {
        match self {
            Self::Subroutine(type_list, _) => {
                if type_list == arg_types {
                    Ok(())
                } else if type_list.len() != arg_types.len() {
                    Err(ParserError::IncorrectFunctionArity(
                        type_list.len(),
                        arg_types.len(),
                    ))
                } else {
                    Err(ParserError::Typing(TypeError::ArgumentMismatch(
                        type_list.to_vec(),
                        arg_types.to_vec(),
                    )))
                }
            }
            Self::NativeFunction(fun) => fun.validate(arg_types),
        }
    }
}

impl Typed for CallableTypeSpecifier {
    fn get_type(&self) -> Type {
        match self {
            Self::Subroutine(_, t) => t.clone(),
            Self::NativeFunction(callable) => callable.get_type(),
        }
    }
}

#[derive(Debug)]
pub struct Parser<T>
where
    T: Iterator<Item = Locatable<Token>>,
{
    tokens: Peekable<T>,
    function_scope: HashMap<String, CallableTypeSpecifier>, // Functions are G L O B A L
    type_scope: TypeScope, // Used to keep track of variables and their types for variable expressions.
}

impl<T> From<T> for Parser<T>
where
    T: Iterator<Item = Locatable<Token>>,
{
    fn from(tokens: T) -> Self {
        let function_scope = GLOBALS
            .iter()
            .cloned()
            .map(|(name, f)| (name.to_string(), CallableTypeSpecifier::NativeFunction(f)))
            .collect();
        Parser {
            tokens: tokens.peekable(),
            type_scope: HashMap::new(),
            function_scope,
        }
    }
}

type LocResult<T> = Result<Locatable<T>, Locatable<ParserError>>;
// The location field is duplicated so results can be handled using standard error operators.
// Had (Result<T< ParserError>, Location) been used, the `?` operator would not work directly.

macro_rules! recursive_descent {
    ($self:ident, $lhs:ident, $rhs:ident, $($ops:pat) | +, $add:stmt) => {{
        let  (mut expr, loc) = $self.$lhs()?.deconstruct();
        while let Some(kind) = $self.tokens.peek() {
            let loc2 = *kind.loc_ref();
            match kind.deloc_ref() {
                $($ops) | + => {
                    let op_token = $self.tokens.next().unwrap(); //Safe to unwrap: peek ensures a value exists
                    let right = $self.$rhs()?.deloc();
                    $add
                    let op = BinaryOperator::from(op_token.deloc());
                    match op.validate(&expr, &right) {
                        Ok(_) => {
                            expr = Expression::Binary {
                                left: Box::new(expr),
                                op,
                                right: Box::new(right),
                            }
                        }
                        Err(e) => return Err(Locatable::new(ParserError::from(e), loc)),
                    }
                }

                _ => return Ok(Locatable::new(expr, loc2)),
            }
        }

        Ok(Locatable::new(expr, loc))
    }
}}

impl<T> Parser<T>
where
    T: Iterator<Item = Locatable<Token>>,
{
    const UnexpectedEOF: (ParserError, Location) = (
        ParserError::UnexpectedEndOfFile,
        Location { line: 0, column: 0 },
    );

    fn consume(&mut self, kind: Token) -> LocResult<Token> {
        let (test, loc) = self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)?.deconstruct();
        if discriminant(&test) == discriminant(&kind) {
            Ok(Locatable::new(test, loc))
        } else {
            Err(Locatable::new(ParserError::Expected(kind), loc))
        }
    }

    fn check_type(
        &self,
        to_check: &impl Typed,
        type_to_check: Type,
        loc: Location,
    ) -> Result<(), Locatable<ParserError>> {
        let type_of_checkee = to_check.get_type();
        if type_of_checkee != type_to_check {
            Err(Locatable::new(
                ParserError::Typing(TypeError::SingleExpected(type_to_check, type_of_checkee)),
                loc,
            ))
        } else {
            Ok(())
        }
    }

    pub fn expression(&mut self) -> LocResult<Expression> {
        self.assignment()
    }

    fn get_ident(&self, expr: Expression) -> Result<ExprIdentifier, ParserError> {
        match expr {
            Expression::Variable(name, _) => Ok(ExprIdentifier::Variable(name)),
            Expression::Binary {
                left: ident,
                op: BinaryOperator::Index,
                right: index,
            } => {
                let inner_ident = self.get_ident(*ident)?;
                Ok(ExprIdentifier::Index(Box::new(inner_ident), index))
            }
            Expression::Constant(_, _) => Err(ParserError::ConstantsAreConstant),
            _ => Err(ParserError::InvalidAssignmentTarget),
        }
    }

    fn assignment(&mut self) -> LocResult<Expression> {
        let (expr, loc) = self.logical_or()?.deconstruct();
        match self.tokens.peek().map(Locatable::deloc_ref) {
            Some(Token::Equals) => {
                let loc2 = self.tokens.next().unwrap().get_loc();
                let val = self.assignment()?.deloc();
                match expr {
                    Expression::Variable(name, t) => {
                        self.check_type(&val, t, loc2)?;
                        Ok(Locatable::new(
                            Expression::Assignment(ExprIdentifier::Variable(name), Box::new(val)),
                            loc2,
                        ))
                    }
                    Expression::Binary {
                        op: BinaryOperator::Index,
                        ..
                    } => {
                        self.check_type(&val, expr.get_type(), loc2)?;
                        let ident = self.get_ident(expr).map_err(|e| (e, loc2))?;
                        Ok(Locatable::new(Expression::Assignment(ident, Box::new(val)), loc2))
                    }

                    Expression::Constant(_, _) => {
                        Err(Locatable::new(ParserError::ConstantsAreConstant, loc2))
                    }
                    _ => Err(Locatable::new(ParserError::InvalidAssignmentTarget, loc2)),
                }
            }
            _ => Ok(Locatable::new(expr, loc)),
        }
    }

    fn logical_or(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, logical_and, logical_and, Token::Or, ())
    }

    fn logical_and(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, equality, equality, Token::And, ())
    }
    fn equality(&mut self) -> LocResult<Expression> {
        recursive_descent!(
            self,
            comparison,
            comparison,
            Token::DoubleEqual | Token::NotEqual,
            ()
        )
    }

    fn comparison(&mut self) -> LocResult<Expression> {
        recursive_descent!(
            self,
            sum,
            sum,
            Token::LessThan | Token::LessEqual | Token::GreaterThan | Token::GreaterEqual,
            ()
        )
    }

    fn sum(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, factor, factor, Token::Plus | Token::Minus, ())
    }

    fn factor(&mut self) -> LocResult<Expression> {
        recursive_descent!(
            self,
            unary,
            unary,
            Token::Star | Token::Slash | Token::Div | Token::Mod,
            ()
        )
    }

    fn unary(&mut self) -> LocResult<Expression> {
        match self.tokens.peek().map(Locatable::deloc_ref) {
            Some(Token::Not) | Some(Token::Minus) => {
                let (op_token, loc) = self.tokens.next().unwrap().deconstruct();
                let right = self.unary()?.deloc();
                let op = UnaryOperator::from(op_token);
                match op.validate(&right) {
                    Ok(_) => Ok(Locatable::new(Expression::Unary(op, Box::new(right)), loc)),
                    Err(e) => Err(Locatable::new(ParserError::from(e), loc)),
                }
            }
            _ => self.call(),
        }
    }
    fn call(&mut self) -> LocResult<Expression> {
        let (expr, loc) = self.index()?.deconstruct();
        if let Some(Token::LeftParenthesis) = self.tokens.peek().map(Locatable::deloc_ref) {
            self.tokens.next();
            let arguments = if let Some(Token::RightParenthesis) = self.tokens.peek().map(Locatable::deloc_ref) {
                self.tokens.next();
                Vec::new()
            } else {
                self.comma_separated_values(Token::RightParenthesis, Parser::expression)?.deloc()
            };
            match expr {
                Expression::Subroutine(name, t) | Expression::NativeFunction(name, t) => {
                    let argument_type_list = arguments.iter().map(Typed::get_type).collect();
                    let call_typer = self
                        .function_scope
                        .get(&name)
                        .expect("already checked when identifier is parsed");
                    call_typer
                        .validate(&argument_type_list)
                        .map_err(|e| (e, loc))?;
                    //TODO: Validate argument length and type
                    return Ok(Locatable::new(
                        Expression::Call {
                            callee: name,
                            args: arguments,
                            return_type: t,
                        },
                        loc,
                    ));
                }
                _ => {
                    return Err(Locatable::new(ParserError::NotACallable, loc));
                }
            }
        }
        Ok(Locatable::new(expr, loc))
    }

    fn index(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, primary, expression, Token::LeftBracket, {
            self.consume(Token::RightBracket)?;
        })
    }

    fn primary(&mut self) -> LocResult<Expression> {
        match self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)?.deconstruct() {
            (Token::Literal(lit), loc) => Ok(Locatable::new(Expression::Literal(lit), loc)),
            (Token::LeftParenthesis, _) => {
                let (expr, loc) = self.expression()?.deconstruct();
                match self.tokens.peek().map(Locatable::deloc_ref) {
                    Some(Token::RightParenthesis) => {
                        self.tokens.next();
                        Ok(Locatable::new(Expression::Grouping(Box::new(expr)), loc))
                    }
                    _ => Err(Locatable::new(
                        ParserError::Expected(Token::RightParenthesis),
                        loc,
                    )),
                }
            }
            (Token::LeftBracket, _) => self.list(),
            (Token::ConstIdentifier(name), loc) => match self.type_scope.get(&name) {
                Some(t) => Ok(Locatable::new(Expression::Constant(name, t.clone()), loc)),
                None => match self.function_scope.get(&name) {
                    Some(spec) => Ok(Locatable::new(Expression::NativeFunction(name, spec.get_type()), loc)),
                    None => Err(Locatable::new(ParserError::UndefinedConstant(name), loc)),
                },
            },
            (Token::VarIdentifier(name), loc) => match self.type_scope.get(&name) {
                Some(t) => Ok(Locatable::new(Expression::Variable(name, t.clone()), loc)),
                None => match self.function_scope.get(&name) {
                    Some(spec) => Ok(Locatable::new(Expression::Subroutine(name, spec.get_type()), loc)),
                    None => Err(Locatable::new(ParserError::UndefinedVariable(name), loc)),
                },
            },

            (_, loc) => Err(Locatable::new(ParserError::ExpectedExpression, loc)),
        }
    }

    fn list(&mut self) -> LocResult<Expression> {
        let (list_elements, loc) =
            self.comma_separated_values(Token::RightBracket, Parser::expression)?.deconstruct();
        let first_lit_type = list_elements
            .iter()
            .next()
            .expect("At least one element in the list")
            .get_type();
        if list_elements
            .iter()
            .all(|lit| lit.get_type() == first_lit_type)
        {
            Ok(Locatable::new(Expression::Array(list_elements), loc))
        } else {
            Err(Locatable::new(ParserError::DifferentArrayTypes, loc))
        }
    }

    fn comma_separated_values<Val>(
        &mut self,
        terminator: Token,
        value_function: fn(&mut Self) -> LocResult<Val>,
    ) -> LocResult<Vec<Val>> {
        let mut expression_list = Vec::new();
        expression_list.push(value_function(self)?.deloc());
        while let Some((kind, loc)) = self.tokens.next().map(Locatable::deconstruct) {
            if discriminant(&kind) == discriminant(&Token::Comma) {
                expression_list.push(value_function(self)?.deloc());
            } else if discriminant(&kind) == discriminant(&terminator) {
                return Ok(Locatable::new(expression_list, loc));
            } else {
                return Err(Locatable::new(
                    ParserError::ExpectedOneOf(vec![Token::Comma, terminator]),
                    loc,
                ));
            }
        }
        Err(Locatable::from(Parser::<T>::UnexpectedEOF))
    }
    fn expression_statement(&mut self) -> LocResult<Statement> {
        let (expr, loc) = self.expression()?.deconstruct();
        self.consume(Token::SemiColon)?;
        Ok(Locatable::new(Statement::Expression(expr), loc))
    }
    fn var_declaration(&mut self) -> LocResult<Statement> {
        self.consume(Token::Colon)?;
        let var_type = self.type_hint()?.deloc();
        let (var_name, loc) = match self.consume(Token::VarIdentifier(String::new()))?.deconstruct() {
            (Token::VarIdentifier(var_name), loc) => (var_name, loc),
            _ => unreachable!("consume should have checked type"),
        };
        self.consume(Token::Equals)?;
        let var_val = self.expression()?.deloc();
        self.consume(Token::SemiColon)?;
        self.check_type(&var_val, var_type.clone(), loc)?;
        if self.function_scope.contains_key(&var_name) {
            Err(Locatable::new(ParserError::AlreadyDefinedAsFunction, loc))
        } else {
            self.type_scope.insert(var_name.clone(), var_type);
            Ok(Locatable::new(Statement::VarDeclaration(var_name, var_val), loc))
        }
    }
    fn constant_declaration(&mut self) -> LocResult<Statement> {
        self.consume(Token::Colon)?;
        let const_type = self.type_hint()?.deloc();
        let (const_name, loc) = match self.consume(Token::ConstIdentifier(String::new()))?.deconstruct() {
            (Token::ConstIdentifier(const_name), loc) => (const_name, loc),
            _ => unreachable!("consume should have checked type"),
        };
        self.consume(Token::Equals)?;
        let const_val = self.expression()?.deloc();
        self.consume(Token::SemiColon)?;
        self.check_type(&const_val, const_type.clone(), loc)?;
        if self.function_scope.contains_key(&const_name) {
            Err(Locatable::new(ParserError::AlreadyDefinedAsFunction, loc))
        } else {
            self.type_scope.insert(const_name.clone(), const_type); // Scope allows us to keep track of current variables and their types.
            Ok(Locatable::new(Statement::ConstDeclaraction(const_name, const_val), loc))
        }
    }
    fn subroutine_declaration(&mut self) -> LocResult<Statement> {
        // TODO: Check if defined as variable
        let (subroutine_name, loc) = match self.consume(Token::VarIdentifier(String::new()))?.deconstruct() {
            (Token::VarIdentifier(name), loc) => (name, loc),
            _ => unreachable!("consume checks type"),
        };
        self.consume(Token::LeftParenthesis)?;
        let parameters = if let Some(Token::RightParenthesis) = self.tokens.peek().map(Locatable::deloc_ref) {
            self.tokens.next();
            Vec::new()
        } else {
            self.comma_separated_values(Token::RightParenthesis, Parser::name_type_pair)?.deloc()
        };
        let return_type = if let Some(Token::Arrow) = self.tokens.peek().map(Locatable::deloc_ref) {
            self.tokens.next();
            self.type_hint()?.deloc()
        } else {
            Type::Void
        };
        let mut type_scope_placeholder = HashMap::new();
        for (name, t) in parameters.iter() {
            type_scope_placeholder.insert(name.clone(), t.clone());
        }
        swap(&mut self.type_scope, &mut type_scope_placeholder);
        let parameter_type_list: Vec<Type> = parameters.iter().map(|(_, t)| t).cloned().collect();
        self.function_scope.insert(
            subroutine_name.clone(),
            CallableTypeSpecifier::Subroutine(parameter_type_list, return_type.clone()),
        ); // Inserts own function to allow for recursive functions

        let body = self.block_statement(
            &[Token::EndSubroutine],
            &mut (|s| Parser::local_declaration(s, Some(return_type.clone()))),
        )?.deloc();
        swap(&mut self.type_scope, &mut type_scope_placeholder);
        self.consume(Token::EndSubroutine)?;

        if return_type != Type::Void && !body.iter().any(Parser::<T>::is_return_statement) {
            // Although some static checking is done, subroutines cannot be guaranteed to always
            // return a value.
            // Example: A function with a while loop with a condition that is never met, and a
            // return statement inside the body.
            // Thus, dynamic checking is also implemented
            Err(Locatable::new(ParserError::SubroutineRequiresReturn, loc))
        } else {
            Ok(Locatable::new(
                Statement::SubroutineDeclaration {
                    name: subroutine_name,
                    parameters,
                    return_type,
                    body,
                },
                loc,
            ))
        }
    }

    fn is_return_statement(statement: &Statement) -> bool {
        match statement {
            Statement::Return(_) => true,
            Statement::Expression(_)
            | Statement::VarDeclaration(_, _)
            | Statement::ConstDeclaraction(_, _) => false,
            Statement::If { body, .. }
            | Statement::While { body, .. }
            | Statement::DoWhile { body, .. }
            | Statement::For { body, .. } => body.iter().any(Parser::<T>::is_return_statement),
            Statement::SubroutineDeclaration { .. } => unreachable!("No inner subroutines"),
        }
    }

    fn name_type_pair(&mut self) -> LocResult<(String, Type)> {
        let (name, _) = match self.consume(Token::VarIdentifier(String::new()))?.deconstruct() {
            (Token::VarIdentifier(name), loc) => (name, loc),
            _ => unreachable!("consume checks type"),
        };
        self.consume(Token::Colon)?;
        let (parameter_type, loc) = self.type_hint()?.deconstruct();
        Ok(Locatable::new((name, parameter_type), loc))
    }

    fn block_statement(
        &mut self,
        end_tok: &[Token],
        fallback: &mut dyn FnMut(&mut Self) -> LocResult<Statement>,
    ) -> LocResult<Vec<Statement>> {
        let mut block = Vec::new();
        while let Some((kind, loc)) = self.tokens.peek().map(Locatable::deconstruct_ref) {
            if end_tok
                .iter()
                .any(|t| discriminant(t) == discriminant(kind))
            {
                // End token kept in stream
                return Ok(Locatable::new(block, *loc));
            } else {
                let stmt = fallback(self)?;
                block.push(stmt.deloc())
            }
        }
        Err(Locatable::from(Parser::<T>::UnexpectedEOF))
    }

    fn type_hint(&mut self) -> LocResult<Type> {
        // What allows us to identify type errors
        match self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)?.deconstruct() {
            (t @ Token::Int, loc)
            | (t @ Token::Real, loc)
            | (t @ Token::Bool, loc)
            | (t @ Token::Char, loc)
            | (t @ Token::String, loc) => {
                let (mut cur_type, loc) = (Type::from(t), loc);
                while let Token::LeftBracket =
                    self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)?.deloc_ref()
                {
                    self.tokens.next();
                    self.consume(Token::RightBracket)?;
                    cur_type = Type::List(Box::new(cur_type));
                }
                Ok(Locatable::new(cur_type, loc))
            }
            (_, loc) => Err(Locatable::new(
                ParserError::ExpectedOneOf(vec![
                    Token::Int,
                    Token::Real,
                    Token::Bool,
                    Token::Char,
                    Token::String,
                ]),
                loc,
            )),
        }
    }

    fn if_statement(&mut self, return_type: Option<Type>) -> LocResult<Statement> {
        let (condition, loc) = self.expression()?.deconstruct();
        self.check_type(&condition, Type::Boolean, loc)?;
        self.consume(Token::Then)?;
        let body = self.block_statement(&[Token::Else, Token::EndIf], &mut |s| {
            Parser::statement(s, return_type.clone())
        })?.deloc();
        let alternative =
            if let Token::Else = self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)?.deloc_ref() {
                self.tokens.next();
                Some(self.block_statement(&[Token::EndIf], &mut |s| {
                    Parser::statement(s, return_type.clone())
                })?)
            } else {
                None
            };
        self.consume(Token::EndIf)?;
        Ok(Locatable::new(
            Statement::If {
                condition,
                body,
                alternative: alternative.map(Locatable::deloc),
            },
            loc,
        ))
    }

    fn while_statement(&mut self, return_type: Option<Type>) -> LocResult<Statement> {
        let (condition, loc) = self.expression()?.deconstruct();
        self.check_type(&condition, Type::Boolean, loc)?;
        self.consume(Token::Do)?;
        let body = self.block_statement(&[Token::EndWhile], &mut |s| {
            Parser::statement(s, return_type.clone())
        })?.deloc();
        self.consume(Token::EndWhile)?;
        Ok(Locatable::new(Statement::While { condition, body }, loc))
    }

    fn do_while_statement(&mut self, return_type: Option<Type>) -> LocResult<Statement> {
        let body = self.block_statement(&[Token::While], &mut |s| {
            Parser::statement(s, return_type.clone())
        })?.deloc();
        self.consume(Token::While)?;
        let (condition, loc) = self.expression()?.deconstruct();
        self.check_type(&condition, Type::Boolean, loc)?;
        self.consume(Token::EndDoWhile)?;
        Ok(Locatable::new(Statement::DoWhile { condition, body }, loc))
    }

    fn for_statement(&mut self, return_type: Option<Type>) -> LocResult<Statement> {
        let (loop_var, loc) = match self.consume(Token::VarIdentifier(String::new()))?.deconstruct() {
            (Token::VarIdentifier(loop_var), loc) => (loop_var, loc),
            _ => unreachable!("consume checks type"),
        };
        self.consume(Token::Equals)?;
        let (initial_val, val_loc) = self.expression()?.deconstruct();
        self.check_type(&initial_val, Type::Integer, val_loc)?;
        self.consume(Token::To)?;
        let (end_val, end_loc) = self.expression()?.deconstruct();
        self.check_type(&end_val, Type::Integer, end_loc)?;
        let step_val = {
            if let Token::Step = self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)?.deloc_ref() {
                self.tokens.next();
                let (val, loc) = self.expression()?.deconstruct();
                self.check_type(&val, Type::Integer, loc)?;
                val
            } else {
                Expression::Literal(Literal::Integer(1))
            }
        };
        self.consume(Token::Do)?;
        self.type_scope.insert(loop_var.clone(), Type::Integer); // Adds loop variable to scope temporarily.
        let body = self.block_statement(&[Token::EndFor], &mut |s| {
            Parser::statement(s, return_type.clone())
        })?.deloc();
        self.consume(Token::EndFor)?;
        self.type_scope.remove(&loop_var);
        Ok(Locatable::new(
            Statement::For {
                loop_var,
                initial_val,
                end_val,
                step_val,
                body,
            },
            loc,
        ))
    }

    fn statement(&mut self, return_type: Option<Type>) -> LocResult<Statement> {
        // Statements are allowed in any sort of statement block.
        match self.tokens.peek().map(Locatable::deloc_ref).ok_or(Parser::<T>::UnexpectedEOF)? {
            Token::If => {
                self.tokens.next();
                self.if_statement(return_type)
            }
            Token::While => {
                self.tokens.next();
                self.while_statement(return_type)
            }
            Token::Do => {
                self.tokens.next();
                self.do_while_statement(return_type)
            }
            Token::For => {
                self.tokens.next();
                self.for_statement(return_type)
            }
            Token::Return => {
                let loc = self.tokens.next().unwrap().get_loc();
                let return_type = return_type.ok_or((ParserError::ReturnOutsideSubroutine, loc))?;
                self.return_statement(return_type)
            }
            _ => self.expression_statement(),
        }
    }

    fn local_declaration(&mut self, return_type: Option<Type>) -> LocResult<Statement> {
        // Local declarations are only allowed in functions and the global program.
        match self.tokens.peek().map(Locatable::deloc_ref).ok_or(Parser::<T>::UnexpectedEOF)? {
            Token::Var => {
                self.tokens.next();
                self.var_declaration()
            }
            Token::Constant => {
                self.tokens.next();
                self.constant_declaration()
            }
            _ => self.statement(return_type),
        }
    }

    fn return_statement(&mut self, return_type: Type) -> LocResult<Statement> {
        let (expr, loc) = if let Some(Token::SemiColon) = self.tokens.peek().map(Locatable::deloc_ref) {
            let loc_semicolon = self.consume(Token::SemiColon)?.get_loc();
            (Expression::Literal(Literal::Void), loc_semicolon)
        } else {
            let val = self.expression()?;
            self.consume(Token::SemiColon)?;
            val.deconstruct()
        };
        let expr_type = expr.get_type();
        if expr_type == return_type {
            Ok(Locatable::new(Statement::Return(expr), loc))
        } else {
            Err(Locatable::new(
                ParserError::Typing(TypeError::InvalidReturnType(return_type, expr_type)),
                loc,
            ))
        }
    }

    fn global_declaration(&mut self) -> LocResult<Statement> {
        // Global declarations i.e function declarations are only allowed in the global program.
        // This means functions cannot be defined inside functions.
        match self.tokens.peek().map(Locatable::deloc_ref).ok_or(Parser::<T>::UnexpectedEOF)? {
            Token::Subroutine => {
                self.tokens.next();
                self.subroutine_declaration()
            }
            _ => self.local_declaration(None),
        }
    }
}

impl<T> Iterator for Parser<T>
where
    T: Iterator<Item = Locatable<Token>>,
{
    type Item = LocResult<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.peek()?; // Checks if there are any more tokens. If there are none, returns none.
        Some(self.global_declaration())
    }
}
