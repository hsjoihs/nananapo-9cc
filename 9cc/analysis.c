#include "9cc.h"

#define Env AnalysisResult

// リテラルを探す
static int	get_str_literal_index(Env *env, char *str, int len)
{
	t_str_elem	*tmp;

	tmp = env->str_literals;
	while (tmp != NULL)
	{
		if (len == tmp->len && strncmp(tmp->str, str, len) == 0)
			return (tmp->index);
		tmp = tmp->next;
	}

	tmp = calloc(1, sizeof(t_str_elem));
	tmp->str = str;
	tmp->len = len;
	if (env->str_literals == NULL)
		tmp->index = 0;
	else
		tmp->index = env->str_literals->index + 1;

	tmp->next = env->str_literals;
	env->str_literals = tmp;

	return (tmp->index);
}


// LVar
LVar		*find_lvar(Env *env, char *str, int len);
LVar		*create_local_var(Env *env, char *name, int len, Type *type, bool is_arg);


static Node	*get_function_by_name(Env *env, char *name, int len)
{
	int i = 0;
	while (env->func_defs[i])
	{
		Node *tmp = env->func_defs[i];
		if (tmp->flen == len && strncmp(tmp->fname, name, len) == 0)
			return tmp;
		i++;
	}

	i = 0;
	while (env->func_protos[i])
	{
		Node *tmp = env->func_protos[i];
		if (tmp->flen == len && strncmp(tmp->fname, name, len) == 0)
			return tmp;
		i++;
	}
	return NULL;
}


Stack	*sbstack;

SBData	*sbdata_new(bool isswitch, int start, int end)
{
	SBData	*tmp;

	tmp = (SBData *)malloc(sizeof(SBData));
	tmp->isswitch = isswitch;
	tmp->startlabel = start;
	tmp->endlabel = end;

	tmp->type = NULL;
	tmp->cases = NULL;
	tmp->defaultLabel = -1;
	return (tmp);
}

void	sb_forwhile_start(int startlabel, int endlabel)
{
	stack_push(&sbstack, sbdata_new(false, startlabel, endlabel));
}

void	sb_switch_start(Type *type, int endlabel, int defaultLabel)
{
	SBData	*tmp;

	tmp = sbdata_new(true, -1, endlabel);
	tmp->type = type;
	tmp->defaultLabel = defaultLabel;
	stack_push(&sbstack, tmp);
}

SBData	*sb_end(void)
{
	return ((SBData *)stack_pop(&sbstack));
}

SBData	*sb_peek(void)
{
	return (SBData *)stack_peek(sbstack);
}

SBData	*sb_search(bool	isswitch)
{
	Stack	*tmp;
	SBData	*data;

	tmp = sbstack;
	while (tmp != NULL)
	{
		data = (SBData *)tmp->data;
		if (data->isswitch == isswitch)
			return (data);
		tmp = tmp->prev;
	}
	return (NULL);
}

int	switchCaseCount = 0;

static int	add_switchcase(SBData *sbdata, int number)
{
	int			count;
	SwitchCase	*tmp;

	count = switchCaseCount++;

	tmp = (SwitchCase *)malloc(sizeof(SwitchCase));
	tmp->value = number;
	tmp->label = count;
	tmp->next = sbdata->cases;
	sbdata->cases = tmp;

	//TODO 被りチェック

	return (count);
}

static ANode	*global_var(Env *env, Node *node)
{
	int	i;

	if (!is_declarable_type(type))
		error_at(env->token->str, "宣言できない型の変数です");

	i = -1;
	while (env->global_vars[++i])
		continue;
	env->global_vars[i] = node;

	// TODO 代入は定数じゃないとだめ
}

// TODO 型が存在するかのチェック
// TODO consume系は型が存在しなくてもconsumeするようにする -> 予約語でないことを確認したい

static ANode	*struct_block(Env *env, Node *node)
{
	// メンバーのチェック
	
		// 型のサイズを取得
		typesize = type_size(type);
		if (typesize == -1)
			error_at(ident->str, "型のサイズが確定していません");

		maxsize = max_type_size(type);

		// offsetをoffset + typesizeに設定
		if (def->members ==  NULL)
			tmp->offset = typesize;
		else
		{
			i = def->members->offset;
			if (maxsize < 4)
			{
				if (i % 4 + typesize > 4)
					tmp->offset = ((i + 4) / 4 * 4) + typesize;
				else
					tmp->offset = i + typesize;
			}
			else if (maxsize == 4)
				tmp->offset = ((i + 3) / 4) * 4 + typesize;
			else
				tmp->offset = ((i + 7) / 8) * 8 + typesize;
		}

		printf("#  OFFSET OF %s : %d\n", strndup(ident->str, ident->len), tmp->offset);

	// メモリサイズを決定
	if (def->members == NULL)
		def->mem_size = 0;
	else
	{
		maxsize = max_type_size(new_struct_type(env, def->name, def->name_len));
		printf("#  MAX_SIZE = %d\n", maxsize);
		def->mem_size = align_to(def->members->offset, maxsize);
	}
	printf("#  MEMSIZE = %d\n", def->mem_size);

	// offsetを修正
	for (tmp = def->members; tmp != NULL; tmp = tmp->next)
	{
		tmp->offset -= type_size(tmp->type);
	}
}

static ANode	*funcdef(Env *env, Node *node)
{
	int		i;
	LVar	*lvar;
	Type	*type;

	// 引数をチェック
	type = node->arg_type;
	for (i = 0; i < node->argdef_count; i++)
	{
		// 宣言できる型かどうか確認する
		if (!is_declarable_type(type))
			error_at(env->token->str, "宣言できない型の変数です");

		// 変数を作成
		LVar *lvar = create_local_var(env, arg->str, arg->len, type, true);
		type = type->next;
	}

	// TODO 関数名被り
	// TODO プロトタイプ宣言後の関数定義

	// func_defsらに代入
}

static ANode	*check_assign(Env *env, Node *node)
{
	// 代入可能な型かどうか確かめる。
	if (lhs->type->ty == VOID
	|| rhs->type->ty == VOID)
		error_at(tok->str, "voidを宣言、代入できません");

	if (!type_equal(rhs->type, lhs->type))
	{
		if (type_can_cast(rhs->type, lhs->type, false))
		{
			printf("#assign (%s) <- (%s)\n",
					get_type_name(lhs->type),
					get_type_name(rhs->type));
			node->rhs = new_node(ND_CAST, rhs, NULL);
			rhs = node->rhs;
			rhs->type = lhs->type;
		}
		else
		{
			error_at(tok->str, "左辺(%s)に右辺(%s)を代入できません",
					get_type_name(lhs->type),
					get_type_name(rhs->type));
		}
	}
}

static ANode	*check_add(Env *env, Node *node)
{
	Type	*l;
	Type	*r;

	l = node->lhs->type;
	r = node->rhs->type;

	// 左辺をポインタにする
	if (is_pointer_type(r))
	{
		Type	*tmp;
		tmp = l;
		l = r;
		r = tmp;
	}

	// 両方ともポインタ
	if (is_pointer_type(l)
	&& is_pointer_type(r))
	{
		if (!type_equal(l, r))
			error_at(tok->str, "型が一致しないポインタ型どうしの加減算はできません");
		node->type = new_primitive_type(INT);// TODO size_tにする

		int size = type_size(l->ptr_to);
		if (size == 0 || size > 1)
		{
			if (size == 0)
				fprintf(stderr, "WARNING : サイズ0の型のポインタ型どうしの加減算は未定義動作です");
			node = new_node(ND_DIV, node, new_node_num(size));
			node->type = new_primitive_type(INT);
		}
		return (node);
	}

	// ポインタと整数の演算
	if (is_pointer_type(l)
	&& is_integer_type(r))
	{
		node->type = l;
		return (node);
	}

	// 両方整数なら型の優先順位を考慮
	if (is_integer_type(l)
	|| is_integer_type(r))
	{
		// Intを左に寄せる
		if (r->ty == INT)
		{
			Type	*tmp;
			tmp = l;
			l = r;
			r = tmp;
		}
		// 左辺の型にする
		node->type = l;
		return (node);
	}

	error_at(tok->str, "演算子 +, - が%dと%dの間に定義されていません",
			node->lhs->type->ty, node->rhs->type->ty);
}

static ANode	*check_mul(Env *env, Node *node)
{
	if (!is_integer_type(node->lhs->type)
	|| !is_integer_type(node->rhs->type))
		error_at(tok->str, "ポインタ型に* か / を適用できません");

	node->type = new_primitive_type(INT);
}

static ANode	*check_call(Env *env, Node *node)
{
	Node *refunc = get_function_by_name(env, node->fname, node->flen);
	// 関数定義が見つからない場合エラー
	if (refunc == NULL)
		error_at(env->token->str, "warning : 関数%sがみつかりません\n", strndup(node->fname, node->flen));

	// 引数の数を確認
	if (node->argdef_count != refunc->argdef_count)
		error_at(env->token->str, "関数%sの引数の数が一致しません\n expected : %d\n actual : %d", strndup(node->fname, node->flen), refunc->argdef_count, node->argdef_count);

	// 引数の型を比べる
	LVar *def = refunc->locals;

	for (int i = 0; i < node->argdef_count; i++)
	{
		// 型の確認
		if (!type_equal(def->type, args->type))
		{
			// 暗黙的なキャストの確認
			if (!type_can_cast(args->type, def->type, false))
				error_at(env->token->str, "関数%sの引数(%s)の型が一致しません\n %s と %s",
						strndup(node->fname, node->flen),
						strndup(def->name, def->len),
						get_type_name(def->type),
						get_type_name(args->type));

			Node *cast = new_node(ND_CAST, args, NULL);
			cast->type = def->type;
			cast->next = args->next;
			args = cast;
		}

		// きっとuse->localsは使われないので使ってしまう
		args->locals = def;

		// 格納
		if (node->args == NULL)
			node->args = args;
		else
		{
			Node *tmp = node->args;
			for (int j = 0; j < i - 1; j++)
				tmp = tmp->next;
			tmp->next = args;
		}

		// 進める
		def = def->next;
		args = args->next;
	}

	// 型を返り値の型に設定
	node->type = refunc->ret_type;
}

static ANode	*primary(Env *env, Node *node)
{
	switch (node->kind)
	{
		case ND_CAST:
			if (!type_can_cast(node->lhs->type, type_cast, true))
				error_at(env->token->str, "%sを%sにキャストできません", get_type_name(node->lhs->type), get_type_name(type_cast));
			break;
		case ND_PARENTHESES:
			break;
		case ND_LVAR:
			LVar	*lvar = find_lvar(env, tok->str, tok->len);
			if (lvar != NULL)
			{
				node = new_node(ND_LVAR, NULL, NULL);
				node->offset = lvar->offset;
				node->type = lvar->type;
			}
			else
			{
				Node *glovar = find_global(env, tok->str, tok->len);
				if (glovar != NULL)
				{
					node = new_node(ND_LVAR_GLOBAL, NULL, NULL);
					node->var_name = glovar->var_name;
					node->var_name_len = glovar->var_name_len;
					node->type = glovar->type;
				}
				else
					error_at(tok->str,
						"%sが定義されていません",
						strndup(tok->str, tok->len));
			}
			break;
		case ND_STR_LITERAL:
			break;
		case ND_CHAR_LITERAL:
			break;
		case ND_NUM:
			break;
	}
}

static ANode	*suffix(Env *env, Node *node)
{
	switch (node->kind)
	{
		case ND_STRUCT_PTR_VALUE:
			if (!is_pointer_type(node->type) || node->type->ptr_to->ty != STRUCT)
				error_at(env->token->str, "struct*ではありません");
		
			elem = struct_get_member(node->type->ptr_to->strct, ident->str, ident->len);
			if (elem == NULL)
				error_at(env->token->str, "識別子が存在しません", strndup(ident->str, ident->len));
			// TODO いろいろチェックと保存
			break;
		case ND_STRUCT_VALUE:
			if (node->type->ty != STRUCT)
				error_at(env->token->str, "structではありません");
			elem = struct_get_member(node->type->strct, ident->str, ident->len);
			if (elem == NULL)
				error_at(env->token->str, "識別子が存在しません", strndup(ident->str, ident->len));
			break;
		case ND_SUBSCRIPT:
			// 左にポインタを寄せる
			if (is_pointer_type(add->rhs->type))
			{
				Node	*tmp;
					tmp = add->lhs;
				add->lhs = add->rhs;
				add->rhs = tmp;
			}
	
			if (!is_pointer_type(add->lhs->type))
				error_at(env->token->str, "ポインタ型ではありません");
			if (!is_integer_type(add->rhs->type))
				error_at(env->token->str, "添字の型が整数ではありません");
			add->type = add->lhs->type;

			break;
		case ND_SUFFIX_INCREMENT:
			break;
		case ND_SUFFIX_DECREMENT:
			node = create_add(false, node, new_node_num(1), env->token);
			node->kind = ND_COMP_SUB;
			node = create_add(true, node, new_node_num(1), env->token); // 1を足す
			break;
		case ND_CALL:
			break;
	}
}

static ANode	*expr(Env *env, Node *node)
{
	switch (node->kind)
	{
		case ND_ASSIGN:
			break;
		case ND_COMP_ADD:
			break;
		case ND_COMP_SUB:
			break;
		case ND_COMP_MUL:
			break;
		case ND_COMP_DIV:
			break;
		case ND_COMP_MOD:
			break;
		case ND_COND_ADD:
			// TODO 型チェック?
			if (!is_integer_type(node->lhs)
			|| !is_integer_type(node->rhs))
				error_at(env->token->str, "左辺か右辺の型が整数型ではありません (&&)");
			break;
		case ND_COND_OR:
			// TODO 型チェック?
			break;
		case ND_EQUAL:
			// TODO  型チェック
			if (!can_compared(node->lhs->type, node->rhs->type))
				error_at(env->token->str,
						"%sと%sを比較することはできません",
						get_type_name(node->lhs->type), get_type_name(node->rhs->type));
			break;
		case ND_NEQUAL:
			// TODO  型チェック
			if (!can_compared(node->lhs->type, node->rhs->type))
				error_at(env->token->str,
						"%sと%sを比較することはできません",
						get_type_name(node->lhs->type), get_type_name(node->rhs->type));
			break;
		case ND_LESS:
			// TODO  型チェック
			if (!can_compared(node->lhs->type, node->rhs->type))
				error_at(env->token->str,
						"%sと%sを比較することはできません",
						get_type_name(node->lhs->type), get_type_name(node->rhs->type));
			break;
		case ND_LESSEQ:
			// TODO  型チェック
			if (!can_compared(node->lhs->type, node->rhs->type))
				error_at(env->token->str,
						"%sと%sを比較することはできません",
						get_type_name(node->lhs->type), get_type_name(node->rhs->type));
			break;
		case ND_ADD:
			// TODO チェック
			break;
		case ND_SUB:
			// TODO チェック
			break;
		case ND_UNARY_ADD:
			// TODO チェック
			if (is_pointer_type(node->type))
				error_at(env->token->str, "ポインタ型に対してunary -を適用できません");
			break;
		case ND_UNARY_MINUS:
			// TODO チェック
			if (is_pointer_type(node->rhs->type))
				error_at(env->token->str, "ポインタ型に対してunary -を適用できません");
			node->type = node->rhs->type;
			break;
		case ND_DEREF:
			// TODO チェック
			if (!is_pointer_type(node->lhs->type))
				error_at(env->token->str,
						"ポインタではない型(%d)に対してunary *を適用できません",
						node->lhs->type->ty);
			node->type = node->lhs->type->ptr_to;	
			break;
		case ND_ADDR:
			// TODO 左辺値かの検証はしていない
			// 変数と構造体、ND_DEREFに対する&
			if (node->lhs->kind != ND_LVAR
			&& node->lhs->kind != ND_LVAR_GLOBAL
			&& node->lhs->kind != ND_STRUCT_VALUE
			&& node->lhs->kind != ND_STRUCT_PTR_VALUE
			&& node->lhs->kind != ND_DEREF) // TODO 文字列リテラルは？
				error_at(env->token->str, "変数以外に&演算子を適用できません Kind: %d", node->lhs->kind);

			node->type = new_type_ptr_to(node->lhs->type);
			break;
		case ND_COMP_ADD:
			// ハイ
			break;
		case ND_COMP_SUB:
			// ハイハイ
			break;
		case ND_NEGATIVE:
			node = unary(env);
			node = new_node(ND_EQUAL, node, new_node_num(0));
			node->type = new_primitive_type(INT);
			break;
		case ND_SIZEOF:
			// TODO
			break;
		case ND_PREFIX_INCREMENT:
			break;
		case ND_PREFIX_DECREMENT:
			break;
		default:
			suffix(env, node);
			break;
	}
}

static ANode	*stmt(Env *env, Node *node)
{
	switch (node->kind)
	{
		case ND_RETURN:
			// TODO 型チェック
			break;
		case ND_IF:
			// TODO 型チェック
			break;
		case ND_WHILE:
			// TODO 型チェック
			sb_forwhile_start(-1, -1);
			sb_end();
			break;
		case ND_DOWHILE:
			// TODO 型チェック
			sb_forwhile_start(-1, -1);
			sb_end();
			break;
		case ND_FOR:
			// TODO 型チェック
			sb_forwhile_start(-1, -1);
			sb_end();
			break;
		case ND_SWITCH:
			// TODO exprの型チェック, キャストも
			if (!is_integer_type(node->lhs->type))
				error_at(env->token->str, "switch文で整数型以外の型で分岐することはできません");

			sb_switch_start(node->lhs->type, -1, -1);
			SBData	*data = sb_end();
			node->switch_cases = data->cases;
			node->switch_has_default = data->defaultLabel != -1;
			break;
		case ND_CASE:
			// TODO 型チェック
			SBData	*sbdata = sb_search(true);
			if (sbdata == NULL)
				error_at(env->token->str, "caseに対応するswitchがありません");
			int label = add_switchcase(sbdata, number);
			node->switch_label = label;
			break;
		case ND_BREAK:
			SBData	*sbdata = sb_peek();
			if (sbdata == NULL)
				error_at(env->token->str, "breakに対応するstatementがありません");
			break;
		case ND_CONTINUE:
			if (sb_search(false) == NULL)
				error_at(env->token->str, "continueに対応するstatementがありません");
			break;
		case ND_DEFAULT:
			SBData *data = sb_search(true);
			if (data == NULL)
				error_at(env->token->str, "defaultに対応するstatementがありません");
			if (data->defaultLabel != -1)
				error_at(env->token->str, "defaultが2個以上あります");

			data->defaultLabel = 1;
			break;
		case ND_BLOCK:
			// TODO 型チェック
			break;
		case ND_DEFVAR:
			if (!is_declarable_type(type))
				error_at(env->token->str, "宣言できない型の変数です");

			LVar *created = create_local_var(env, tok->str, tok->len, type, false);
			node->offset = created->offset;
			break;
		default:
			expr(env, node)
	}
}

static ANode	*program(Env *env, Node *node)
{
	switch(node->kind)
	{
		case ND_STRUCT_DEF:
			break;
		case ND_FUNCDEF:
		case ND_PROTOTYPE:
			break;
		case ND_DEFVAR_GLOBAL:
			break;
		default:
			stmt(env, node);
	}
}

static void	analyze_code(Env *env)
{
	int	i;

	for (i = 0; env->parse->code[i]; i++)
	{
		env->code[i] = filescope(env, env->parse->code[i]);
	}
}

Env *analyze(ParseResult *parseres)
{
	Env		*res;

	res = calloc(1, sizeof(Env));
	res->parse = parseres;
	program(res);
	return (res);
}
