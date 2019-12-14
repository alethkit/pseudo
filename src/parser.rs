use super::ast::{
    callable::NativeFunction,
    expression::{ExprIdentifier, Expression},
    literal::Literal,
    location::Location,
    operator::{BinaryOperator, UnaryOperator},
    statement::Statement,
    token::Token,
    types::{Type, TypeError, Typed},
};
use std::collections::HashMap;
use std::iter::Peekable;
use std::mem::{discriminant, swap};

type TypeScope = HashMap<String, Type>;

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

pub struct Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    tokens: Peekable<T>,
    function_scope: HashMap<String, CallableTypeSpecifier>, // Functions are G L O B A L
    type_scope: TypeScope, // Used to keep track of variables and their types for variable expressions.
}

impl<T> From<T> for Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    fn from(tokens: T) -> Self {
        let mut function_scope = HashMap::new();
        function_scope.insert(
            "LEN".to_owned(),
            CallableTypeSpecifier::NativeFunction(NativeFunction::Len),
        );
        Parser {
            tokens: tokens.peekable(),
            type_scope: HashMap::new(),
            function_scope,
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    Typing(TypeError),
    ExpectedExpression,
    Expected(Token),
    ExpectedOneOf(Vec<Token>),
    UnexpectedEndOfFile,
    UndefinedVariable(String),
    UndefinedConstant(String),
    DifferentArrayTypes,
    ConstantsAreConstant, // A special case is added, if users forget that constants are constant.
    InvalidAssignmentTarget,
    NotACallable,
    IncorrectFunctionArity(usize, usize), // Wrong number of arguments passed
    AlreadyDefinedAsFunction,
    SubroutineRequiresReturn,
}

impl From<TypeError> for ParserError {
    fn from(err: TypeError) -> Self {
        ParserError::Typing(err)
    }
}

type LocResult<T> = Result<(T, Location), (ParserError, Location)>;
// The location field is duplicated so results can be handled using standard error operators.
// Had (Result<T< ParserError>, Location) been used, the `?` operator would not work directly.

macro_rules! recursive_descent {
    ($self:ident, $lhs:ident, $rhs:ident, $($ops:pat) | +, $add:stmt) => {{
        let (mut expr, loc) = $self.$lhs()?;
        while let Some((kind, loc2)) = $self.tokens.peek() {
            match kind {
                $($ops) | + => {
                    let (op_token, loc) = $self.tokens.next().unwrap(); //Safe to unwrap: peek ensures a value exists
                    let right = $self.$rhs()?.0;
                    $add
                    let op = BinaryOperator::from(op_token);
                    println!("Left: {:#?}", expr);
                    println!("Right: {:#?}", right);
                    match op.validate(&expr, &right) {
                        Ok(_) => {
                            expr = Expression::Binary {
                                left: Box::new(expr),
                                op,
                                right: Box::new(right),
                            }
                        }
                        Err(e) => return Err((ParserError::from(e), loc)),
                    }
                }

                _ => return Ok((expr, *loc2)),
            }
        }

        Ok((expr, loc))
    }
}}

impl<T> Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    const UnexpectedEOF: (ParserError, Location) = (
        ParserError::UnexpectedEndOfFile,
        Location { line: 0, column: 0 },
    );

    fn consume(&mut self, kind: Token) -> LocResult<Token> {
        let (test, loc) = self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)?;
        if discriminant(&test) == discriminant(&kind) {
            Ok((test, loc))
        } else {
            Err((ParserError::Expected(kind), loc))
        }
    }

    fn check_type(
        &self,
        to_check: &impl Typed,
        type_to_check: Type,
        loc: Location,
    ) -> Result<(), (ParserError, Location)> {
        let type_of_checkee = to_check.get_type();
        if type_of_checkee != type_to_check {
            Err((
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
        //TODO: Get array assignment to work
        let (expr, loc) = self.logical_or()?;
        match self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::Equals, _) => {
                let (_, loc2) = self.tokens.next().unwrap();
                let (val, _) = self.assignment()?;
                println!("{:#?}", expr);
                match expr {
                    Expression::Variable(name, t) => {
                        self.check_type(&val, t, loc2)?;
                        Ok((
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
                        Ok((Expression::Assignment(ident, Box::new(val)), loc2))
                    }

                    Expression::Constant(_, _) => Err((ParserError::ConstantsAreConstant, loc2)),
                    _ => Err((ParserError::InvalidAssignmentTarget, loc2)),
                }
            }
            _ => Ok((expr, loc)),
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
        match self.tokens.peek() {
            Some((Token::Not, _)) | Some((Token::Minus, _)) => {
                let (op_token, loc) = self.tokens.next().unwrap();
                let right = self.unary()?.0;
                let op = UnaryOperator::from(op_token);
                match op.validate(&right) {
                    Ok(_) => Ok((Expression::Unary(op, Box::new(right)), loc)),
                    Err(e) => Err((ParserError::from(e), loc)),
                }
            }
            _ => self.call(),
        }
    }
    fn call(&mut self) -> LocResult<Expression> {
        let (expr, loc) = self.index()?;
        if let Some((Token::LeftParenthesis, _)) = self.tokens.peek() {
            println!("call time!");
            println!("{:#?}", expr);
            let (_, _) = self.tokens.next().unwrap();
            let arguments = if let Some((Token::RightParenthesis, _)) = self.tokens.peek() {
                self.tokens.next();
                Vec::new()
            } else {
                self.comma_separated_values(Token::RightParenthesis, Parser::expression)?
                    .0
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
                    return Ok((
                        Expression::Call {
                            callee: name,
                            args: arguments,
                            return_type: t,
                        },
                        loc,
                    ));
                }
                _ => {
                    return Err((ParserError::NotACallable, loc));
                }
            }
        }
        Ok((expr, loc))
    }

    fn index(&mut self) -> LocResult<Expression> {
        recursive_descent!(self, primary, expression, Token::LeftBracket, {
            self.consume(Token::RightBracket)?;
        })
    }

    fn primary(&mut self) -> LocResult<Expression> {
        match self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::Literal(lit), loc) => Ok((Expression::Literal(lit), loc)),
            (Token::LeftParenthesis, _) => {
                let (expr, loc) = self.expression()?;
                match self.tokens.peek() {
                    Some((Token::RightParenthesis, _)) => {
                        self.tokens.next();
                        Ok((Expression::Grouping(Box::new(expr)), loc))
                    }
                    _ => Err((ParserError::Expected(Token::RightParenthesis), loc)),
                }
            }
            (Token::LeftBracket, _) => self.list(),
            (Token::ConstIdentifier(name), loc) => match self.type_scope.get(&name) {
                Some(t) => Ok((Expression::Constant(name, t.clone()), loc)),
                None => match self.function_scope.get(&name) {
                    Some(spec) => Ok((Expression::NativeFunction(name, spec.get_type()), loc)),
                    None => Err((ParserError::UndefinedConstant(name), loc)),
                },
            },
            (Token::VarIdentifier(name), loc) => match self.type_scope.get(&name) {
                Some(t) => Ok((Expression::Variable(name, t.clone()), loc)),
                None => match self.function_scope.get(&name) {
                    Some(spec) => Ok((Expression::Subroutine(name, spec.get_type()), loc)),
                    None => Err((ParserError::UndefinedVariable(name), loc)),
                },
            },

            (_, loc) => Err((ParserError::ExpectedExpression, loc)),
        }
    }

    fn list(&mut self) -> LocResult<Expression> {
        let (list_elements, loc) =
            self.comma_separated_values(Token::RightBracket, Parser::expression)?;
        let first_lit_type = list_elements
            .iter()
            .next()
            .expect("At least one element in the list")
            .get_type();
        if list_elements
            .iter()
            .all(|lit| lit.get_type() == first_lit_type)
        {
            Ok((Expression::Array(list_elements), loc))
        } else {
            Err((ParserError::DifferentArrayTypes, loc))
        }
    }

    fn comma_separated_values<Val>(
        &mut self,
        terminator: Token,
        value_function: fn(&mut Self) -> LocResult<Val>,
    ) -> LocResult<Vec<Val>> {
        let mut expression_list = Vec::new();
        expression_list.push(value_function(self)?.0);
        while let Some((kind, loc)) = self.tokens.next() {
            if discriminant(&kind) == discriminant(&Token::Comma) {
                expression_list.push(value_function(self)?.0);
            } else if discriminant(&kind) == discriminant(&terminator) {
                return Ok((expression_list, loc));
            } else {
                return Err((
                    ParserError::ExpectedOneOf(vec![Token::Comma, terminator]),
                    loc,
                ));
            }
        }
        Err(Parser::<T>::UnexpectedEOF)
    }
    fn expression_statement(&mut self) -> LocResult<Statement> {
        let (expr, loc) = self.expression()?;
        self.consume(Token::SemiColon)?;
        Ok((Statement::Expression(expr), loc))
    }
    fn var_declaration(&mut self) -> LocResult<Statement> {
        self.consume(Token::Colon)?;
        let (var_type, _) = self.type_hint()?;
        let (var_name, loc) = match self.consume(Token::VarIdentifier("".to_string()))? {
            (Token::VarIdentifier(var_name), loc) => (var_name, loc),
            _ => unreachable!("consume should have checked type"),
        };
        self.consume(Token::Equals)?;
        let (var_val, _) = self.expression()?;
        self.consume(Token::SemiColon)?;
        println!("{:#?}", var_val);
        self.check_type(&var_val, var_type.clone(), loc)?;
        if self.function_scope.contains_key(&var_name) {
            Err((ParserError::AlreadyDefinedAsFunction, loc))
        } else {
            self.type_scope.insert(var_name.clone(), var_type);
            Ok((Statement::VarDeclaration(var_name, var_val), loc))
        }
    }
    fn constant_declaration(&mut self) -> LocResult<Statement> {
        self.consume(Token::Colon)?;
        let (const_type, _) = self.type_hint()?;
        let (const_name, loc) = match self.consume(Token::ConstIdentifier(String::new()))? {
            (Token::ConstIdentifier(const_name), loc) => (const_name, loc),
            _ => unreachable!("consume should have checked type"),
        };
        self.consume(Token::Equals)?;
        let (const_val, _) = self.expression()?;
        self.consume(Token::SemiColon)?;
        self.check_type(&const_val, const_type.clone(), loc)?;
        if self.function_scope.contains_key(&const_name) {
            Err((ParserError::AlreadyDefinedAsFunction, loc))
        } else {
            self.type_scope.insert(const_name.clone(), const_type); // Scope allows us to keep track of current variables and their types.
            Ok((Statement::ConstDeclaraction(const_name, const_val), loc))
        }
    }
    fn subroutine_declaration(&mut self) -> LocResult<Statement> {
        //TODO: check that if a function has a return type, there is one return statement
        let (subroutine_name, loc) = match self.consume(Token::VarIdentifier(String::new()))? {
            (Token::VarIdentifier(name), loc) => (name, loc),
            _ => unreachable!("consume checks type"),
        };
        self.consume(Token::LeftParenthesis)?;
        println!(
            "{:#?}",
            self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)?.0
        );
        let parameters = if let Some((Token::RightParenthesis, _)) = self.tokens.peek() {
            self.tokens.next();
            Vec::new()
        } else {
            self.comma_separated_values(Token::RightParenthesis, Parser::name_type_pair)?
                .0
        };
        let return_type = if let Some((Token::Arrow, _)) = self.tokens.peek() {
            self.tokens.next();
            self.type_hint()?.0
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

        let (body, _) = self.block_statement(
            &[Token::EndSubroutine],
            &mut (|s| Parser::function_statement(s, return_type.clone())),
        )?;
        swap(&mut self.type_scope, &mut type_scope_placeholder);
        self.consume(Token::EndSubroutine)?;
        self.consume(Token::SemiColon)?;

        let return_discriminant =
            discriminant(&Statement::Return(Expression::Literal(Literal::Integer(0)))); // Exists to compare with discriminant
        if return_type != Type::Void
            && body
                .iter()
                .all(|stmt| discriminant(stmt) != return_discriminant)
        {
            Err((ParserError::SubroutineRequiresReturn, loc))
        } else {
            Ok((
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

    fn name_type_pair(&mut self) -> LocResult<(String, Type)> {
        let (name, _) = match self.consume(Token::VarIdentifier(String::new()))? {
            (Token::VarIdentifier(name), loc) => (name, loc),
            _ => unreachable!("consume checks type"),
        };
        self.consume(Token::Colon)?;
        let (parameter_type, loc) = self.type_hint()?;
        Ok(((name, parameter_type), loc))
    }

    fn block_statement(
        &mut self,
        end_tok: &[Token],
        fallback: &mut dyn FnMut(&mut Self) -> LocResult<Statement>,
    ) -> LocResult<Vec<Statement>> {
        let mut block = Vec::new();
        while let Some((kind, loc)) = self.tokens.peek() {
            println!("token: {:#?}", kind);
            if end_tok
                .iter()
                .any(|t| discriminant(t) == discriminant(kind))
            {
                // End token kept in stream
                return Ok((block, *loc));
            } else {
                let stmt = fallback(self)?;
                block.push(stmt.0)
            }
        }
        Err(Parser::<T>::UnexpectedEOF)
    }

    fn type_hint(&mut self) -> LocResult<Type> {
        // What allows us to identify type errors
        match self.tokens.next().ok_or(Parser::<T>::UnexpectedEOF)? {
            (t @ Token::Int, loc)
            | (t @ Token::Real, loc)
            | (t @ Token::Bool, loc)
            | (t @ Token::Char, loc)
            | (t @ Token::String, loc) => {
                let (mut cur_type, loc) = (Type::from(t), loc);
                while let (Token::LeftBracket, _) =
                    self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)?
                {
                    self.tokens.next();
                    self.consume(Token::RightBracket)?;
                    cur_type = Type::List(Box::new(cur_type));
                }
                Ok((cur_type, loc))
            }
            (_, loc) => Err((
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

    fn if_statement(&mut self) -> LocResult<Statement> {
        let (condition, loc) = self.expression()?;
        self.check_type(&condition, Type::Boolean, loc)?;
        self.consume(Token::Then)?;
        let (body, _) =
            self.block_statement(&[Token::Else, Token::EndIf], &mut Parser::statement)?;
        let alternative =
            if let (Token::Else, _) = self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
                self.tokens.next();
                Some(self.block_statement(&[Token::EndIf], &mut Parser::statement)?)
            } else {
                None
            };
        self.consume(Token::EndIf)?;
        Ok((
            Statement::If {
                condition,
                body,
                alternative: alternative.map(|t| t.0),
            },
            loc,
        ))
    }

    fn while_statement(&mut self) -> LocResult<Statement> {
        let (condition, loc) = self.expression()?;
        self.check_type(&condition, Type::Boolean, loc)?;
        self.consume(Token::Do)?;
        let (body, _) = self.block_statement(&[Token::EndWhile], &mut Parser::statement)?;
        self.consume(Token::EndWhile)?;
        Ok((Statement::While { condition, body }, loc))
    }

    fn do_while_statement(&mut self) -> LocResult<Statement> {
        let (body, _) = self.block_statement(&[Token::While], &mut Parser::statement)?;
        self.consume(Token::While)?;
        let (condition, loc) = self.expression()?;
        self.check_type(&condition, Type::Boolean, loc)?;
        self.consume(Token::EndDoWhile)?;
        Ok((Statement::DoWhile { condition, body }, loc))
    }

    fn for_statement(&mut self) -> LocResult<Statement> {
        let (loop_var, loc) = match self.consume(Token::VarIdentifier(String::new()))? {
            (Token::VarIdentifier(loop_var), loc) => (loop_var, loc),
            _ => unreachable!("consume checks type"),
        };
        self.consume(Token::Equals)?;
        let (initial_val, val_loc) = self.expression()?;
        self.check_type(&initial_val, Type::Integer, val_loc)?;
        self.consume(Token::To)?;
        let (end_val, end_loc) = self.expression()?;
        self.check_type(&end_val, Type::Integer, end_loc)?;
        let step_val = {
            if let (Token::Step, _) = self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
                self.tokens.next();
                let (val, loc) = self.expression()?;
                self.check_type(&val, Type::Integer, loc)?;
                val
            } else {
                Expression::Literal(Literal::Integer(1))
            }
        };
        self.consume(Token::Do)?;
        self.type_scope.insert(loop_var.clone(), Type::Integer); // Adds loop variable to scope temporarily.
        let (body, _) = self.block_statement(&[Token::EndFor], &mut Parser::statement)?;
        self.consume(Token::EndFor)?;
        self.type_scope.remove(&loop_var);
        Ok((
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

    fn statement(&mut self) -> LocResult<Statement> {
        // Statements are allowed in any sort of statement block.
        match self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::If, _) => {
                self.tokens.next();
                self.if_statement()
            }
            (Token::While, _) => {
                self.tokens.next();
                self.while_statement()
            }
            (Token::Do, _) => {
                self.tokens.next();
                self.do_while_statement()
            }
            (Token::For, _) => {
                self.tokens.next();
                self.for_statement()
            }
            _ => self.expression_statement(),
        }
    }

    fn local_declaration(&mut self) -> LocResult<Statement> {
        // Local declarations are only allowed in functions and the global program.
        match self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::Var, _) => {
                self.tokens.next();
                self.var_declaration()
            }
            (Token::Constant, _) => {
                self.tokens.next();
                self.constant_declaration()
            }
            _ => self.statement(),
        }
    }

    fn function_statement(&mut self, return_type: Type) -> LocResult<Statement> {
        // Includes local declarations and return statements
        match self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::Return, _) => {
                self.tokens.next();
                self.return_statement(return_type)
            }
            _ => self.local_declaration(),
        }
    }

    fn return_statement(&mut self, return_type: Type) -> LocResult<Statement> {
        let (expr, loc) = if let Some((Token::SemiColon,_)) = self.tokens.peek() {
            let (_, loc_semicolon) = self.consume(Token::SemiColon)?;
            (Expression::Literal(Literal::Void), loc_semicolon)
        } else {
            self.expression()?
        };
        let expr_type = expr.get_type();
        if expr_type == return_type {
            Ok((Statement::Return(expr), loc))
        } else {
            Err((
                ParserError::Typing(TypeError::InvalidReturnType(return_type, expr_type)),
                loc,
            ))
        }
    }

    fn global_declaration(&mut self) -> LocResult<Statement> {
        // Global declarations i.e function declarations are only allowed in the global program.
        // This means functions cannot be defined inside functions.
        match self.tokens.peek().ok_or(Parser::<T>::UnexpectedEOF)? {
            (Token::Subroutine, _) => {
                self.tokens.next();
                self.subroutine_declaration()
            }
            _ => self.local_declaration(),
        }
    }
}

impl<T> Iterator for Parser<T>
where
    T: Iterator<Item = (Token, Location)>,
{
    type Item = LocResult<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.peek()?; // Checks if there are any more tokens. If there are none, returns none.
        Some(self.global_declaration())
    }
}
