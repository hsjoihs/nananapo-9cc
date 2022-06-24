#include "9cc.h"
#include "parse.h"
#include "stack.h"

#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define Env ParseResult

// Node
static Node	*expr(Env *env);
static Node *unary(Env *env);

Node *new_node(NodeKind kind, Node *lhs, Node *rhs)
{
	Node *node = calloc(1, sizeof(Node));
	node->kind = kind;
	node->lhs = lhs;
	node->rhs = rhs;

	// for
	node->for_init = NULL;
	node->for_if = NULL;
	node->for_next = NULL;

	// else
	node->els = NULL;
	
	// call & deffunc
	node->fname = NULL;
	node->args = NULL;

	node->type = NULL;
	return node;
}

Node *new_node_num(int val)
{
	Node *node = new_node(ND_NUM, NULL, NULL);
	node->val = val;

	//if (val < 127 && val > -128)
	//	node->type = new_primitive_type(CHAR);
	//else
		node->type = new_primitive_type(INT);
	return node;
}

static Node *call(Env *env, Token *tok)
{
	Node	*node;
	Node	*args;

	node  = new_node(ND_CALL, NULL, NULL);
	node->fname = tok->str;
	node->flen = tok->len;
	node->args = NULL;
	node->argdef_count = 0;

	args = NULL;

	if (!consume(env, ")"))
	{
		Node *arg = expr(env);
		args = arg;
		node->argdef_count = 1;

		for (;;)
		{	
			if (consume(env, ")"))
				break;
			if (!consume(env, ","))
				error_at(env->token->str, "トークンが,ではありません");
			
			arg = expr(env);
			arg->next = NULL;
			if (args == NULL)
				args = arg;
			else
			{
				for (Node *tmp = args; tmp; tmp = tmp->next)
				{
					if (tmp->next == NULL)
					{
						tmp->next = arg;
						break ;
					}
				}
			}
			node->argdef_count += 1;
		}
	}

	return node;
}

// 後置インクリメント, デクリメント
static Node	*read_suffix_increment(Env *env, Node *node)
{
	if (consume(env, "++"))
		node = new_node(ND_SUFFIX_INCREMENT, node, NULL);
	else if (consume(env, "--"))
		node = new_node(ND_SUFFIX_DECREMENT, node, NULL);
	return (node);
}

// 添字によるDEREF
static Node	*read_deref_index(Env *env, Node *node)
{
	while (consume(env, "["))
	{
		// SUBSCRIPT
		Node	*add = new_node(ND_ADD, node, expr(env));

		node = new_node(ND_DEREF, add, NULL);
		node->type = node->lhs->type->ptr_to;

		if (!consume(env, "]"))
			error_at(env->token->str, "%s");
	}
	return read_suffix_increment(env, node);
}

static Node *primary(Env *env)
{
	Token	*tok;
	Node	*node;
	Type	*type_cast;

	// 括弧
	if (consume(env, "("))
	{
		// 型を読む
		type_cast = consume_type_before(env);
		
		// 括弧の中身が型ではないなら優先順位を上げる括弧
		if (type_cast == NULL)
		{
			node = new_node(ND_PARENTHESES, expr(env), NULL);
			node->type = node->lhs->type;
			expect(env, ")");
			return read_deref_index(env, node);
		}

		// 明示的なキャスト
		// TODO キャストの優先順位が違う
		expect(env, ")");
		node = new_node(ND_CAST, unary(env), NULL);
		node->type = type_cast;
		return read_deref_index(env, node);
	}
	
	// identかcall
	tok = consume_ident(env);
	if (tok)
	{
		// call func
		if (consume(env, "("))
			node = call(env, tok);
		// use ident
		else
		{
			// TODO
		}
		return read_deref_index(env, node);
	}

	// string
	tok = consume_str_literal(env);
	if (tok)
	{
		node = new_node(ND_STR_LITERAL, NULL, NULL);
		node->str_index = get_str_literal_index(env, tok->str, tok->len);
		node->type = new_type_ptr_to(new_primitive_type(CHAR));
		return read_deref_index(env, node);
	}

	// char
	tok = consume_char_literal(env);
	if (tok)
	{
		// TODO ND_CHAR_LITERAL
		node = new_node_num(get_char_to_int(tok->str, tok->strlen_actual));
		node->type = new_primitive_type(CHAR);
		return read_deref_index(env, node); // charの後ろに[]はおかしいけれど、とりあえず許容
	}

	// 数
	int number;
	if (!consume_number(env, &number))
		error_at(env->token->str, "数字が必要です");
	node = new_node_num(number);

	return read_deref_index(env, node);
}

static Node	*arrow_loop(Env *env, Node *node)
{
	Token				*ident;

	if (consume(env, "->"))
	{
		ident = consume_ident(env);

		if (ident == NULL)
			error_at(env->token->str, "識別子が必要です");
		
		node = new_node(ND_STRUCT_PTR_VALUE, node, NULL);
		node = read_deref_index(env, node);

		return arrow_loop(env, node);
	}
	else if (consume(env, "."))
	{
		ident = consume_ident(env);
		if (ident == NULL)
			error_at(env->token->str, "識別子が必要です");

		node = new_node(ND_STRUCT_VALUE, node, NULL);
		node = read_deref_index(env, node);

		return arrow_loop(env, node);
	}
	else
		return (node);
}

static Node	*arrow(Env *env)
{
	Node	*node;

	node = primary(env);
	return (arrow_loop(env, node));
}

static Node *unary(Env *env)
{
	Node	*node;

	if (consume(env, "+"))
	{
		node = unary(env);
		// TODO unaryaddにする
		return node;
	}
	else if (consume(env, "-"))
	{
		node = new_node(ND_SUB, new_node_num(0), unary(env));
		return node;
	}
	else if (consume(env, "*"))
	{
		node = new_node(ND_DEREF, unary(env), NULL);
		return node;
	}
	else if (consume(env, "&"))
	{
		node = new_node(ND_ADDR, unary(env), NULL);
		return node;
	}
	else if (consume(env, "++"))
	{
		node = new_node(ND_PREFIX_INCREMENT, unary(env), NULL);
		return (node);
	}
	else if (consume(env, "--"))
	{
		node = new_node(ND_PREFIX_DECREMENT, unary(env), NULL);
		return (node);
	}
	else if (consume(env, "!"))
	{
		// TODO
		return (node);
	}
	else if (consume_with_type(env, TK_SIZEOF))
	{
		// TODO 型か式
		node = unary(env);
		node = new_node_num(type_size(node->type));
		return node;
	}

	return arrow(env);
}

static Node *mul(Env *env)
{
	Node *node = unary(env);
	for (;;)
	{
		if (consume(env, "*"))
			node = create_mul(0, node, unary(env), env->token);
		else if (consume(env, "/"))
			node = create_mul(1, node, unary(env), env->token);
		else if (consume(env, "%"))
			node = create_mul(2, node, unary(env), env->token);
		else
			return node;
	}
}

static Node *add(Env *env)
{
	Node *node = mul(env);
	for (;;)
	{
		if (consume(env, "+"))
			node = new_node(ND_ADD, node, mul(env));
		else if (consume(env, "-"))
			node = new_node(ND_SUB, node, mul(env));
		else
			return (node);
	}
}

static Node *relational(Env *env)
{
	Node *node = add(env);
	for (;;)
	{
		if (consume(env, "<"))
			node = new_node(ND_LESS, node, add(env));
		else if (consume(env, "<="))
			node = new_node(ND_LESSEQ, node, add(env));
		else if (consume(env, ">"))
			node = new_node(ND_LESS, add(env), node);
		else if (consume(env, ">="))
			node = new_node(ND_LESSEQ, add(env), node);
		else
			return node;

		// TODO 返り値はINTでいいの？
		node->type = new_primitive_type(INT);
	}
}

static Node *equality(Env *env)
{
	Node *node = relational(env);
	for (;;)
	{
		if (consume(env, "=="))
			node = new_node(ND_EQUAL, node, relational(env));
		else if (consume(env, "!="))
			node = new_node(ND_NEQUAL, node, relational(env));
		else
			return node;

		// TODO 返り値はINTでいいの？
		node->type = new_primitive_type(INT);
	}
}

static Node	*conditional(Env *env)
{
	Node	*node;

	node = equality(env);
	if (consume(env, "&&"))
	{
		// TODO 返り値はINTでいいの？
		node = new_node(ND_COND_AND, node, conditional(env));
		node->type = new_primitive_type(INT);
	}
	else if (consume(env, "||"))
	{
		// TODO 返り値はINTでいいの？
		node = new_node(ND_COND_OR, node, conditional(env));
		node->type = new_primitive_type(INT);
	}
	return (node);
}

static Node	*assign(Env *env)
{
	Node	*node;

	node = conditional(env);
	if (consume(env, "="))
		node = new_node(ND_ASSIGN, node, assign(env));
	else if (consume(env, "+="))
		node = new_node(ND_COMP_ADD, node, assign(env));
	else if (consume(env, "-="))
		node = new_node(ND_COMP_SUB, node, assign(env));
	else if (consume(env, "*="))
		node = new_node(ND_COMP_MUL, node, assign(env));
	else if (consume(env, "/="))
		node = new_node(ND_COMP_DIV, node, assign(env));
	else if (consume(env, "%="))
		node = new_node(ND_COMP_MOD, node, assign(env));
	return (node);
}

static Node	*expr(Env *env)
{
	return assign(env);
}

// TODO 条件の中身がintegerか確認する
static Node	*stmt(Env *env)
{
	Node	*node;

	if (consume_with_type(env, TK_RETURN))
	{
		if (consume(env, ";"))
		{
			node = new_node(ND_RETURN, NULL, NULL);
			return (node);
		}
		else
			node = new_node(ND_RETURN, expr(env), NULL);
	}
	else if (consume_with_type(env, TK_IF))
	{
		if (!consume(env, "("))
			error_at(env->token->str, "(ではないトークンです");
		node = new_node(ND_IF, expr(env), NULL);
		if (!consume(env, ")"))
			error_at(env->token->str, ")ではないトークンです");
		node->rhs = stmt(env);
		if (consume_with_type(env, TK_ELSE)) // TODO else if
			node->els = stmt(env);
		return node;
	}
	else if (consume_with_type(env, TK_WHILE))
	{
		if (!consume(env, "("))
			error_at(env->token->str, "(ではないトークンです");
		node = new_node(ND_WHILE, expr(env), NULL);
		if (!consume(env, ")"))
			error_at(env->token->str, ")ではないトークンです");

		if (!consume(env, ";"))
			node->rhs = stmt(env);

		return node;
	}
	else if (consume_with_type(env, TK_DO))
	{
		node = new_node(ND_DOWHILE, stmt(env), NULL);

		if (!consume_with_type(env, TK_WHILE))
			error_at(env->token->str, "whileが必要です");

		if (!consume(env, "("))
			error_at(env->token->str, "(ではないトークンです");
		node->rhs = expr(env);
		if (!consume(env, ")"))
			error_at(env->token->str, ")ではないトークンです");
	}
	else if (consume_with_type(env, TK_FOR))
	{
		if (!consume(env, "("))
			error_at(env->token->str, "(ではないトークンです");
		node = new_node(ND_FOR, NULL, NULL);
		// for init
		if (!consume(env, ";"))
		{
			node->for_init = expr(env);
			if (!consume(env, ";"))
				error_at(env->token->str, ";が必要です");
		}
		// for if
		if (!consume(env, ";"))
		{
			node->for_if = expr(env);
			if (!consume(env, ";"))
				error_at(env->token->str, ";が必要です");
		}
		// for next
		if (!consume(env, ")"))
		{
			node->for_next = expr(env);
			if(!consume(env, ")"))
				error_at(env->token->str, ")ではないトークンです");
		}

		// stmt
		if (!consume(env, ";"))
			node->lhs = stmt(env);

		return node;
	}
	else if (consume_with_type(env, TK_SWITCH))
	{
		if (!consume(env, "("))
			error_at(env->token->str, "(ではないトークンです");

		node = new_node(ND_SWITCH, expr(env), NULL);
		if (!consume(env, ")"))
			error_at(env->token->str, ")ではないトークンです");

		node->rhs = stmt(env);
		return (node);
	}
	else if (consume_with_type(env, TK_CASE))
	{
		int	number;

		if (!consume_number(env, &number))
			error_at(env->token->str, "数値が必要です");
		if (!consume(env, ":"))
			error_at(env->token->str, ":が必要です");

		node = new_node(ND_CASE, NULL, NULL);
		node->val = number;
		return (node);
	}
	else if (consume_with_type(env, TK_BREAK))
		node = new_node(ND_BREAK, NULL, NULL);
	else if (consume_with_type(env, TK_CONTINUE))
		node = new_node(ND_CONTINUE, NULL, NULL);
	else if (consume_with_type(env, TK_DEFAULT))
	{
		if (!consume(env, ":"))
			error_at(env->token->str, ":が必要です");
		node = new_node(ND_DEFAULT, NULL, NULL);
		return (node);
	}
	else if (consume(env, "{"))
	{
		node = new_node(ND_BLOCK, NULL, NULL);
		Node *start = node;
		while (!consume(env, "}"))
		{
			node->lhs = stmt(env);
			node->rhs = new_node(ND_BLOCK, NULL, NULL);
			node = node->rhs;
		}
		return start;
	}
	else
	{
		Type	*type = consume_type_before(env);
		if (type != NULL)
		{
			Token *tok = consume_ident(env);

			if (tok == NULL)
				error_at(env->token->str, "識別子が必要です");

			expect_type_after(env, &type);

			// TODO voidチェックは違うパスでやりたい....
			node = new_node(ND_DEFVAR, NULL, NULL);
			node->type = type;

			// 宣言と同時に代入
			if (consume(env, "="))
			{
				node = new_node(ND_LVAR, NULL, NULL);
				node->offset = created->offset;
				node->type = created->type;
				node = create_assign(node, expr(env), env->token);
				// TODO DEFVARにする
				// TODO analysisで変更する
			}
		}
		else
			node = expr(env);
	}
	if(!consume(env, ";"))
		error_at(env->token->str, ";ではないトークン(Kind : %d , %s)です", env->token->kind, strndup(env->token->str, env->token->len));

	return node;
}

static Node	*global_var(Env *env, Type *type, Token *ident)
{
	Node	*node;

	// 後ろの型を読む
	expect_type_after(env, &type);

	node = new_node(ND_DEFVAR_GLOBAL, NULL, NULL);
	node->type = type;
	node->var_name = ident->str;
	node->var_name_len = ident->len;

	// TODO 代入文

	if (!consume(env, ";"))
		error_at(env->token->str, ";が必要です。");

	return node;
}

static Node	*struct_block(Env *env, Token *ident)
{
	Type				*type;
	StructDef			*def;
	StructMemberElem	*tmp;
	int					i;
	int					typesize;
	int					maxsize;

	def = calloc(1, sizeof(StructDef));
	def->name = ident->str;
	def->name_len = ident->len;
	def->mem_size = -1;
	def->members = NULL;

	// TODO defをnodeに格納

	printf("# START READ STRUCT %as\n", strndup(ident->str, ident->len));

	while (1)
	{
		if (consume(env, "}"))
			break;

		type = consume_type_before(env);
		if (type == NULL)
			error_at(env->token->str, "型宣言が必要です");
		ident = consume_ident(env);
		if (ident == NULL)
			error_at(env->token->str, "識別子が必要です");
		expect_type_after(env, &type);
		if (!consume(env, ";"))
			error_at(env->token->str, ";が必要です");

		tmp = calloc(1, sizeof(StructMemberElem));
		tmp->name = ident->str;
		tmp->name_len = ident->len;
		tmp->type = type;
		tmp->next = def->members;
		def->members = tmp;
	}

	if (!consume(env, ";"))
		error_at(env->token->str, ";が必要です");

	return (new_node(ND_STRUCT_DEF, NULL, NULL));
}

// TODO ブロックを抜けたらlocalsを戻す
// TODO 変数名の被りチェックは別のパスで行う
// (まで読んだところから読む
static Node	*funcdef(Env *env, Type *ret_type, Token *ident)
{
	Node	*node;

	// create node
	node = new_node(ND_FUNCDEF, NULL, NULL);
	node->fname = ident->str;
	node->flen = ident->len;
	node->ret_type = ret_type;
	node->argdef_count = 0;

	// args
	if (!consume(env, ")"))
	{
		for (;;)
		{
			// 型宣言の確認
			Type *type = consume_type_before(env);
			if (type == NULL)
				error_at(env->token->str,"型宣言が必要です");

			// 仮引数名
			Token *arg = consume_ident(env);
			if (arg == NULL)
			{
				// voidなら引数0個
				if (type->ty == VOID && node->argdef_count == 0)
				{
					if (!consume(env, ")"))
						error_at(env->token->str, ")が見つかりませんでした。");
					break ;
				}
				error_at(env->token->str, "仮引数が必要です");
			}
			// arrayを読む
			expect_type_after(env, &type);

			// 型情報を保存
			type->next = node->arg_type;
			node->arg_type = type;
			node->argdef_count++;
			
			// )か,
			if (consume(env, ")"))
				break;
			if (!consume(env, ","))
				error_at(env->token->str, ",が必要です");
		}
	}

	printf("# READ ARGS\n");

	if (consume(env, ";"))
		node->kind = ND_PROTOTYPE;
	else
		node->lhs = stmt(env);
	
	printf("# END READ FUNC %s\n", strndup(node->fname, node->flen));

	return node;
}

static Node	*filescope(Env *env)
{
	Token	*ident;
	Type	*ret_type;

	// structの宣言か返り値がstructか
	if (consume_with_type(env, TK_STRUCT))
	{
		ident = consume_ident(env);
		if (ident == NULL)
			error_at(env->token->str, "構造体の識別子が必要です");
			
		if (consume(env, "{"))
			return struct_block(env, ident);

		ret_type = new_struct_type(env, ident->str, ident->len);
		consume_type_ptr(env, &ret_type);
	}
	else
		ret_type = consume_type_before(env);

	// グローバル変数か関数宣言か
	if (ret_type != NULL)
	{
		// ident
		ident = consume_ident(env);
		if (ident == NULL)
			error_at(env->token->str, "不明なトークンです");

		// function definition
		if (consume(env, "("))
			return funcdef(env, ret_type, ident);
		else
			return global_var(env, ret_type, ident);
	}

	error_at(env->token->str, "構文解析に失敗しました[filescope kind:%d]", env->token->kind);
	return (NULL);
}

static void	program(Env *env)
{
	int	i;

	i = 0;
	while (!at_eof(env))
		env->code[i++] = filescope(env);
	env->code[i] = NULL;
}

Env	*parse(Token *tok)
{
	Env	*env;

	env = calloc(1, sizeof(Env));
	env->token = tok;
	program(env);
	return env;
}
