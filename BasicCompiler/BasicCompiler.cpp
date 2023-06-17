#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "AST.h"

enum Token
{
	EOFToken = -1,

	defToken = -2,
	externToken = -3,

	identifierToken = -4,
	numberToken = -5,
};

std::string identifier;
double numberValue;

int GetToken()
{
	static int lastChar = ' ';

	while (isspace(lastChar))
	{
		lastChar = getchar();
	}

	// identifier creation
	if (isalpha(lastChar))
	{
		identifier = lastChar;
		while (isalnum((lastChar = getchar())))
		{
			identifier += lastChar;
		}

		if (identifier == "def")
		{
			return defToken;
		}
		if (identifier == "extern")
		{
			return externToken;
		}

		return identifierToken;
	}

	// number literal creation
	if (isdigit(lastChar) || lastChar == '.')
	{
		std::string number;
		number = lastChar;

		while (isdigit(lastChar) || lastChar == '.')
		{
			number += lastChar;
			lastChar = getchar();
		}

		numberValue = strtod(number.c_str(), 0);
		return numberToken;
	}

	// ignoring comments
	if (lastChar == '#') 
	{
	  // Comment until end of line.

		while(lastChar != EOF && lastChar != '\n' && lastChar != '\r')
		{
			lastChar = getchar();
		}

		if (lastChar != EOF)
		{
			return GetToken();
		}
	}


	if (lastChar == EOF)
	{
		return EOFToken;
	}

	int thisChar = lastChar;
	lastChar = getchar();
	return thisChar;

}

static int currentToken;
static int getNextToken() { return currentToken = GetToken(); }

static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() 
{
	if (!isascii(currentToken))
	return -1;

	// Make sure it's a declared binop.
	int TokPrec = BinopPrecedence[currentToken];
	if (TokPrec <= 0)
	return -1;

	return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) 
{
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) 
{
	LogError(Str);
	return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() 
{
	auto Result = std::make_unique<NumberExprAST>(numberValue);
	getNextToken(); // consume the number
	return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() 
{
	getNextToken(); // eat (.
	auto V = ParseExpression();
	if (!V)
	return nullptr;

	if (currentToken != ')')
	return LogError("expected ')'");
	getNextToken(); // eat ).
	return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() 
{
	std::string IdName = identifier;

	getNextToken(); // eat identifier.

	if (currentToken != '(') // Simple variable ref.
	return std::make_unique<VariableExprAST>(IdName);

	// Call.
	getNextToken(); // eat (
	std::vector<std::unique_ptr<ExprAST>> Args;
	if (currentToken != ')') 
	{
		while (true) 
		{
			if (auto Arg = ParseExpression())
			Args.push_back(std::move(Arg));
			else
			return nullptr;

			if (currentToken == ')')
			break;

			if (currentToken != ',')
			return LogError("Expected ')' or ',' in argument list");
			getNextToken();
		}
	}

	// Eat the ')'.
	getNextToken();

	return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() 
{
	switch (currentToken) 
	{
		default:
		return LogError("unknown token when expecting an expression");
		case identifierToken:
		return ParseIdentifierExpr();
		case numberToken:
		return ParseNumberExpr();
		case '(':
		return ParseParenExpr();
	}
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) 
{
  // If this is a binop, find its precedence.
	while (true) 
	{
		int TokPrec = GetTokPrecedence();

		// If this is a binop that binds at least as tightly as the current binop,
		// consume it, otherwise we are done.
		if (TokPrec < ExprPrec)
			return LHS;

		// Okay, we know this is a binop.
		int BinOp = currentToken;
		getNextToken(); // eat binop

		// Parse the primary expression after the binary operator.
		auto RHS = ParsePrimary();
		if (!RHS)
			return nullptr;

		// If BinOp binds less tightly with RHS than the operator after RHS, let
		// the pending operator take RHS as its LHS.
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) 
		{
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
			return nullptr;
		}

		// Merge LHS/RHS.
		LHS =
			std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
	}
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() 
{
	auto LHS = ParsePrimary();
	if (!LHS)
	return nullptr;

	return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() 
{
	if (currentToken != identifierToken)
	return LogErrorP("Expected function name in prototype");

	std::string FnName = identifier;
	getNextToken();

	if (currentToken != '(')
	return LogErrorP("Expected '(' in prototype");

	std::vector<std::string> ArgNames;
	while (getNextToken() == identifierToken)
	ArgNames.push_back(identifier);
	if (currentToken != ')')
	return LogErrorP("Expected ')' in prototype");

	// success.
	getNextToken(); // eat ')'.

	return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() 
{
	getNextToken(); // eat def.
	auto Proto = ParsePrototype();
	if (!Proto)
	return nullptr;

	if (auto E = ParseExpression())
	return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() 
{
	if (auto E = ParseExpression()) 
	{
		// Make an anonymous proto.
		auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
													std::vector<std::string>());
		return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() 
{
	getNextToken(); // eat extern.
	return ParsePrototype();
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

static void HandleDefinition() 
{
	if (ParseDefinition()) 
	{
		fprintf(stderr, "Parsed a function definition.\n");
	} 
	else 
	{
		// Skip token for error recovery.
		getNextToken();
	}
}

static void HandleExtern() 
{
	if (ParseExtern()) 
	{
		fprintf(stderr, "Parsed an extern\n");
	} 
	else 
	{
		// Skip token for error recovery.
		getNextToken();
	}
}

static void HandleTopLevelExpression() 
{
  // Evaluate a top-level expression into an anonymous function.
	if (ParseTopLevelExpr()) 
	{
		fprintf(stderr, "Parsed a top-level expr\n");
	} 
	else 
	{
		// Skip token for error recovery.
		getNextToken();
	}
}

/// top ::= definition | external | expression | ';'
static void MainLoop() 
{
	while (true) 
	{
		fprintf(stderr, "ready> ");
		switch (currentToken) 	
		{
			case EOFToken:
				return;
			case ';': // ignore top-level semicolons.
				getNextToken();
				break;
			case defToken:
				HandleDefinition();
				break;
			case externToken:
				HandleExtern();
				break;
			default:
				HandleTopLevelExpression();
				break;
		}
	}
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() 
{
	// Install standard binary operators.
	// 1 is lowest precedence.
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['/'] = 40; 
	BinopPrecedence['*'] = 40; 
	BinopPrecedence['%'] = 40; 
	BinopPrecedence['^'] = 50; // highest.

	// Prime the first token.
	fprintf(stderr, "ready> ");
	getNextToken();

	// Run the main "interpreter loop" now.
	MainLoop();

	return 0;
}