#include "9cc.h"
#include "stack.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#define ARG_REG_COUNT 6

static void	expr(Node *node);
static void unary(Node *node);

static int	jumpLabelCount = 0;
static char	*arg_regs[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
static int	stack_count = 0;

extern t_str_elem	*str_literals;

extern Stack	*sbstack;

int	max(int a, int b)
{
	if (a > b)
		return (a);
	return (b);
}

int	min(int a, int b)
{
	if (a < b)
		return (a);
	return (b);
}

int	align_to(int n, int align)
{
	if (align == 0)
		return (n);
	return ((n + align - 1) / align * align);
}

void	push()
{
	stack_count += 8;
	printf("    %s %s # stack= %d -> %d\n", ASM_PUSH, RAX, stack_count - 8, stack_count);
}

void	pushi(int data)
{
	stack_count += 8;
	printf("    %s %d # stack= %d -> %d\n", ASM_PUSH, data, stack_count - 8, stack_count);
}

void	pop(char *reg)
{
	stack_count -= 8;
	printf("    pop %s # stack= %d -> %d\n", reg, stack_count + 8, stack_count);
}

void	mov(char *dst, char *from)
{
	printf("    %s %s, %s\n", ASM_MOV, dst, from);
}

void	movi(char *dst, int i)
{
	printf("    %s %s, %d\n", ASM_MOV, dst, i);
}

void	cmps(char *dst, char *from)
{
	printf("    cmp %s, %s\n", dst, from);
}

// TODO 構造体の比較
// rax, rdi
void	cmp(Type *dst, Type *from)
{
	if (type_equal(dst, from))
	{
		if (dst->ty == PTR || dst->ty == ARRAY)
			cmps(RAX, RDI);
		else if (dst->ty == CHAR || dst->ty == BOOL)
			cmps(AL, DIL);
		else if (dst->ty == INT || dst->ty == ENUM)
			cmps(EAX, EDI);
		return ;
	}
	cmps(RAX, RDI);
}

void	load_global(Node *node)
{
	printf("    mov rax, [rip + _%s@GOTPCREL]\n",
		strndup(node->var_name, node->var_name_len));
}

char	*get_str_literal_name(int index)
{
	char	*tmp;

	tmp = calloc(100, sizeof(char));
	sprintf(tmp, "L_STR_%d", index);
	return (tmp);
}

void	comment(char *c)
{
	printf("# %s\n", c);
}

// RAXからR10(アドレス)に値をストアする
static void	store_value(int size)
{
	if (size == 1)
		mov("byte ptr [r10]", AL);
	else if (size == 2)
		mov("word ptr [r10]", EAX);
	else if (size == 4)
		mov("dword ptr [r10]", EAX);
	else if (size == 8)
		mov("[r10]", RAX);
	else
		error("store_valueに不正なサイズ(%d)の値を渡しています", size);
}

// アドレス(RAX)の先の値をR10(アドレス)にストアする
static void store_ptr(int size, bool minus_step)
{
	int		delta;
	char	*dest;
	char	*from;
	char	*op;

	op = "+";
	if (minus_step)
		op = "-";

	dest = R10;
	
	for (int i = 0; i < size; i += 8)
	{
		delta = size - i;
		if (delta >= 8)
		{
			from = R11;
			if (i != 0)
			{
				printf("    %s %s, [%s + %d]\n", ASM_MOV, from, RAX, i);
				printf("    %s [%s %s %d], %s\n", ASM_MOV, dest, op, i, from);
			}
			else
			{
				printf("    %s %s, [%s]\n", ASM_MOV, from, RAX);
				printf("    %s [%s], %s\n", ASM_MOV, dest, from);
			}
			continue ;
		}
		if (delta >= 4)
		{
			from = R11D;
			if (i != 0)
			{
				printf("    %s %s, %s [%s + %d]\n", ASM_MOV, from, DWORD_PTR, RAX, i);
				printf("    %s %s [%s %s %d], %s\n", ASM_MOV, DWORD_PTR, dest, op,  i, from);
			}
			else
			{
				printf("    %s %s, %s [%s]\n", ASM_MOV, from, DWORD_PTR, RAX);
				printf("    %s %s [%s], %s\n", ASM_MOV, DWORD_PTR, dest, from);
			}
			i += 4;
			delta -= 4;
		}
		if (delta >= 2)
		{
			from = R11W;
			if (i != 0)
			{
				printf("    %s %s, %s [%s + %d]\n", ASM_MOV, from, WORD_PTR, RAX, i);
				printf("    %s %s [%s %s %d], %s\n", ASM_MOV, WORD_PTR, dest, op, i, from);
			}
			else
			{
				printf("    %s %s, %s [%s]\n", ASM_MOV, from, WORD_PTR, RAX);
				printf("    %s %s [%s], %s\n", ASM_MOV, WORD_PTR, dest, from);
			}
			i += 2;
			delta -= 2;
		}
		if (delta >= 1)
		{
			from = R11B;
			if (i != 0)
			{
				printf("    %s %s, %s [%s + %d]\n", ASM_MOV, from, BYTE_PTR, RAX, i);
				printf("    %s %s [%s %s %d], %s\n", ASM_MOV, BYTE_PTR, dest, op, i, from);
			}
			else
			{
				printf("    %s %s, %s [%s]\n", ASM_MOV, from, BYTE_PTR, RAX);
				printf("    %s %s [%s], %s\n", ASM_MOV, BYTE_PTR, dest, from);
			}
			i += 1;
			delta -= 1;
		}
	}
}

static void prologue()
{
	// prologue
	mov(RAX, RBP);
	push();
	mov(RBP, RSP);
}

static void	epilogue()
{
	// epilogue
	mov(RSP, RBP);
	pop(RBP);
	printf("    ret\n");
}

// raxをraxに読み込む
static void	load(Type *type)
{
	
	if (type->ty == PTR)
	{
		mov(RAX, "[rax]");
		return ;
	}
	else if (type->ty == CHAR || type->ty == BOOL)
	{
		printf("    mov al, %s [%s]\n", BYTE_PTR, RAX);
		printf("    movzx %s, %s\n", RAX, AL);
		return ;
	}
	else if (type->ty == INT || type->ty == ENUM)
	{
		printf("    mov %s, %s [%s]\n", EAX, DWORD_PTR, RAX);
		//printf("    movzx %s, %s\n", RAX, EAX);
		return ;
	}
	else if (type->ty == ARRAY)
	{
		return ;
	}
	else if (type->ty == STRUCT)
	{
		// TODO とりあえず8byteまで
		// mov(RAX, "[rax]");
		return ;
	}
}

// 変数のアドレスをraxに移動する
static void	lval(Node *node)
{
	if (node->kind != ND_LVAR
	&& node->kind != ND_LVAR_GLOBAL)
		error("変数ではありません Kind:%d Type:%d", node->kind, node->type->ty);
	
	mov(RAX, RBP);
	if (node->offset > 0)
		printf("    sub %s, %d\n", RAX, node->offset);
	else if (node->offset < 0)
		printf("    add %s, %d\n", RAX, - node->offset);
}

static void	call(Node *node)
{
	int		size;
	int 	rbp_offset;
	int		arg_count = 0;
	int		push_count = 0;
	bool	is_aligned;

	for (Node *tmp = node->args; tmp; tmp = tmp->next)
	{
		arg_count++;

		// TODO int[1000]とかにしたら1000 * 4byteコピーしてしまう問題の修正
		size = type_size(tmp->locals->type);
		printf("# PUSH ARG %s (%d)\n",
			strndup(tmp->locals->name, tmp->locals->len),
			size);

		expr(tmp);

		// TODO long doubleで動かなくなります....
		if (tmp->locals->arg_regindex != -1)
		{
			if (size <= 8)
			{
				push();
				push_count++;
				continue ;
			}
			// size > 8なものは必ずstructであると願います( ;∀;)
			// RAXにアドレスが入っていると想定
			mov(RDI, RAX);
			for (int i = 0; i < size; i += 8)
			{
				if (i == 0)
					printf("    %s %s, [%s]\n",
						ASM_MOV, RAX, RDI);
				else
					printf("    %s %s, [%s + %d]\n",
						ASM_MOV, RAX, RDI, i);
				push();
				push_count++;
			}
		}
		else
		{
			push();
			push_count++;
		}
	}

	// 16 byteアラインチェック
	rbp_offset = 0;
	for (Node *tmp = node->args; tmp; tmp = tmp->next)
	{
		if (tmp->locals->arg_regindex != -1)
			continue ;
		rbp_offset = min(rbp_offset,
						tmp->locals->offset - align_to(type_size(tmp->locals->type), 8) + 8);
	}

	// マイナスなのでプラスにする
	rbp_offset = - rbp_offset;

	is_aligned = (stack_count + rbp_offset + 8) % 16 == 0;
	if (!is_aligned)
		rbp_offset += 8;

	printf("# RBP_OFFSET %d (is_aligned : %d)\n", rbp_offset, is_aligned);

	int	pop_count = 0;
	// 後ろから格納していく
	for (int i = arg_count; i > 0; i--)
	{
		// i番目のtmpを求める
		Node *tmp = node->args;
		for (int j = 1; j < i; j++)
			tmp = tmp->next;

		printf("# POP %s\n", strndup(tmp->locals->name, tmp->locals->len));
	
		// TODO int[1000]とかにしたら1000 * 4byteコピーしてしまう問題の修正
		size = type_size(tmp->locals->type);

		// レジスタに入れる
		if (tmp->locals->arg_regindex != -1)
		{
			if (size <= 8)
			{
				printf("    %s %s, [%s + %d]\n", ASM_MOV, RAX, RSP, pop_count++ * 8);
				mov(arg_regs[tmp->locals->arg_regindex], RAX);
				continue ;
			}
			// size > 8なものは必ずstructであると願います( ;∀;)
			// RAXにアドレスが入っていると想定
			for (int i = size - 8; i >= 0; i -= 8)
			{
				printf("    %s %s, [%s + %d]\n", ASM_MOV,
					arg_regs[tmp->locals->arg_regindex - i / 8],
					 RSP, pop_count++ * 8);
			}
			continue ;
		}

		printf("# OFFSET %d\n", tmp->locals->offset);

		// スタックに積む
		// 必ず8byteアラインなので楽々実装
		size = align_to(size, 8);

		printf("    %s %s, [%s + %d]\n", ASM_MOV, RAX, RSP, pop_count++ * 8);
		mov(R10, RSP);

		printf("    sub %s, %d\n", R10,
				(tmp->locals->offset + 16) + rbp_offset);

		// ptr先を渡すのはSTRUCTだけ
		if (tmp->type->ty != STRUCT)
			store_value(size);
		else
			store_ptr(size, false);
	}

	// rspを移動する
	if (rbp_offset != 0)
	{
		if (!is_aligned)
			comment("aligned + 8");
		printf("    sub rsp, %d # rbp_offset\n", rbp_offset);
	}

	// call
	printf("# CALL RBP_OFFSET: %d\n", rbp_offset);
	if (node->is_variable_argument)
		printf("    mov al, 0\n");
	printf("    call _%s\n", strndup(node->fname, node->flen));

	// rspを元に戻す
	if (rbp_offset != 0)
	{
		printf("    add rsp, %d # rbp_offset\n", rbp_offset);
	}

	// stack_countをあわせる
	printf("    add rsp, %d # pop_count\n", pop_count * 8);
	printf("# POP ALL %d -> %d\n", stack_count, stack_count - pop_count * 8);
	stack_count -= pop_count * 8;

	return;
}

// raxに入っている型fromをtoに変換する
static void	cast(Type *from, Type *to)
{
	int	size1;
	int	size2;

	// 型が同じなら何もしない
	if (type_equal(from, to))
		return ;

	printf("# cast %s -> %s\n", get_type_name(from), get_type_name(to));

	// ポインタからポインタのキャストは何もしない
	if (is_pointer_type(from)
	&& is_pointer_type(to))
		return ;
	
	size1 = type_size(from);
	size2 = type_size(to);

	// ポインタから整数へのキャストは情報を落とす
	if (is_pointer_type(from)
	&& is_integer_type(to))
	{
		pushi(0);
		mov(R10, RSP);
		store_value(size2);
		pop(RAX);
		return ;
	}

	// 整数からポインタは0埋めで拡張
	// (ポインタはunsinged long long intなので)
	if (is_integer_type(from)
	&& is_pointer_type(to))
	{
		pushi(0);
		mov(R10, RSP);
		store_value(size1);
		pop(RAX);
		return ;
	}

	// 整数から整数は符号を考えながらキャスト
	if (is_integer_type(from)
	&& is_integer_type(to))
	{
		// TODO unsigned
		// 符号拡張する
		if (size1 < size2)
		{
			if (size1 == 1)
			{
				printf("    movsx %s, %s # cast %d -> %d\n", RAX, AL, size1, size2);
			}
			else if (size1 == 4)
			{
				printf("    movsx %s, %s # cast %d -> %d\n", RAX, EAX, size1, size2);
			}
			else
				error("8byte -> 8byteのキャストは無い");
		}
		return ;
	}

	error("%sから%sへのキャストが定義されていません", get_type_name(from), get_type_name(to));
}

static void	primary(Node *node)
{
	switch (node->kind)
	{
		case ND_CAST:
			expr(node->lhs);
			cast(node->lhs->type, node->type);
			return;
		case ND_LVAR:
			lval(node);
			load(node->type);
			return;
		case ND_LVAR_GLOBAL:
			load_global(node);
			load(node->type);
			return;
		case ND_STR_LITERAL:
			printf("    %s %s, [rip + %s]\n", ASM_LEA, RAX,
					get_str_literal_name(node->str_index));
			return;
		case ND_CALL:
			call(node);
			return;
		case ND_NUM:
			movi(RAX, node->val);
			return;
		case ND_PROTOTYPE:
		case ND_DEFVAR:
			return;
		case ND_PARENTHESES:
			expr(node->lhs);
			break;
		case ND_STRUCT_DEF:
		case ND_ENUM_DEF:
			return;
		default:
			error("不明なノード kind:%d type:%d", node->kind, node->type->ty);
			break;
	}
}

static void	arrow(Node *node, bool as_addr)
{
	int offset;

	switch (node->kind)
	{
		case ND_STRUCT_VALUE:
		case ND_STRUCT_PTR_VALUE:
			break;
		default:
			return primary(node);
	}

	//printf("#ARROW %d->%d\n", node->kind, node->lhs->kind);

	// arrowかその他の可能性がある
	if (node->lhs->kind == ND_STRUCT_VALUE
			|| node->lhs->kind == ND_STRUCT_PTR_VALUE)
		arrow(node->lhs, node->kind == ND_STRUCT_VALUE);
	else
		expr(node->lhs);

	// offsetを足す
	offset = node->struct_elem->offset;
	printf("    add rax, %d # offset\n", offset);

	// 値として欲しいなら値にする
	if (!as_addr)
		load(node->struct_elem->type);
}

static void unary(Node *node)
{
	switch (node->kind)
	{
		case ND_ADDR:
		case ND_DEREF:
			break ;
		default:
			arrow(node, false);
			return ;
	}

	switch(node->kind)
	{
		case ND_ADDR:
			switch(node->lhs->kind)
			{
				// 変数ならアドレスを取り出す
				case ND_LVAR:
				case ND_LVAR_GLOBAL:
					lval(node->lhs);
					break ;
				// 構造体からのアクセスなら、アドレスなのでそのまま返す
				case ND_STRUCT_VALUE:
				case ND_STRUCT_PTR_VALUE:
					arrow(node->lhs, true);
					break ;
				// ND_DEREFならアドレスで止める
				case ND_DEREF:
					expr(node->lhs->lhs);//ここ！！　↓
					break ;
				default:
					error("ND_ADDRを使えない kind:%d", node->lhs->kind);
					break ;
			}
			break ;
		case ND_DEREF:
			expr(node->lhs);// ここと同じ！！！！！変えるときは注意！！！！！！
			load(node->type);
			break ;
		default:
			break ;
	}
}

static void	mul(Node *node)
{
	switch (node->kind)
	{
		case ND_MUL:
		case ND_DIV:
		case ND_MOD:
			break;
		default:
			unary(node);
			return;
	}

	expr(node->rhs);
	push();
	expr(node->lhs);
	pop("rdi");

// TODO 型に対応
	switch (node->kind)
	{
		case ND_MUL:
			printf("    imul rax, rdi\n");	
/*
			if (node->type->ty == INT)
			{
				printf("    imul eax, edi\n");	
			}
*/
			break ;
		case ND_DIV:
			printf("    cdq\n");
			printf("    idiv edi\n");
			break ;
		case ND_MOD:
			printf("    cdq\n"); // d -> q -> o
			printf("    idiv edi\n");
			mov(RAX, RDX);
			break ;
		default:
			break ;
	}
}

static void add_check_pointer(Type *restype, Node **lhs, Node **rhs, bool can_replace)
{
	// 結果がポインタ型なら
	if (restype->ty == PTR || restype->ty == ARRAY)
	{
		// 左辺をポインタ型にする
		if (can_replace &&
			((*rhs)->type->ty == PTR || (*rhs)->type->ty == ARRAY))
		{
			Node *tmp = *lhs;
			*lhs = *rhs;
			*rhs = tmp;
		}

		// 右辺が整数型なら掛け算に置き換える
		if (is_integer_type((*rhs)->type))
		{
			Node *size_node = new_node(ND_NUM, NULL, NULL);
			size_node->val = type_size((*lhs)->type->ptr_to);
			size_node->type = new_primitive_type(INT);
			// 掛け算
			*rhs = new_node(ND_MUL, *rhs, size_node);
			(*rhs)->type = (*lhs)->type; // TODO
			// INT
			size_node->type = new_primitive_type(INT);
		}
	}
}

static void	add(Node *node)
{
	switch (node->kind)
	{
		case ND_ADD:
		case ND_SUB:
			break;
		default:
			mul(node);
			return;
	}

	add_check_pointer(node->type, &node->lhs, &node->rhs, node->kind == ND_ADD);

	expr(node->rhs);
	push();
	expr(node->lhs);
	pop("rdi");

	switch (node->kind)
	{
		case ND_ADD:
			printf("    add rax, rdi\n");
			break;
		case ND_SUB:
			printf("    sub rax, rdi\n");
			break;
		default:
			break;
	}
}

static void relational(Node *node)
{
	switch (node->kind)
	{
		case ND_LESS:
		case ND_LESSEQ:
			break;
		default:
			add(node);
			return;
	}

	expr(node->rhs);
	push();
	expr(node->lhs);
	pop("rdi");

	switch (node->kind)
	{
		case ND_LESS:
			cmp(node->lhs->type, node->rhs->type);
			printf("    setl al\n");
			printf("    movzx rax, al\n");
			break;
		case ND_LESSEQ:
			cmp(node->lhs->type, node->rhs->type);
			printf("    setle al\n");
			printf("    movzx rax, al\n");
			break;
		default:
			break;
	}
}

static void	equality(Node *node)
{
	switch (node->kind)
	{
		case ND_EQUAL:
		case ND_NEQUAL:
			break;
		default:
			relational(node);
			return;
	}

	expr(node->rhs);
	push();
	expr(node->lhs);
	pop("rdi");

	switch (node->kind)
	{
		case ND_EQUAL:
			cmp(node->lhs->type, node->rhs->type);
			printf("    sete al\n");
			printf("    movzx rax, al\n");
			break;
		case ND_NEQUAL:
			cmp(node->lhs->type, node->rhs->type);
			printf("    setne al\n");
			printf("    movzx rax, al\n");
			break;
		default:
			break;
	}
}

static void	conditional(Node *node)
{
	int	lend;

	if (node->kind != ND_COND_AND
		&& node->kind != ND_COND_OR)
	{
		equality(node);
		return ;
	}

	//printf("condand : %d, %d:%d\n", node->kind == ND_COND_AND, node->lhs->kind, node->rhs->kind);

	if (node->kind == ND_COND_AND)
	{
		lend = jumpLabelCount++;

		// 左辺を評価
		equality(node->lhs);
		mov(RDI, "0");
		cmp(node->lhs->type, new_primitive_type(INT));
		printf("    setne al\n"); // 0と等しくないかをalに格納
		printf("    je .Lcond%d\n", lend); // 0ならスキップ

		push();

		// 右辺を評価
		conditional(node->rhs);
		mov(RDI, "0");
		cmp(node->rhs->type, new_primitive_type(INT));
		printf("    setne al\n"); // 0と等しくないかをalに格納

		pop(RDI);
		printf("    add %s, %s\n", RAX, RDI);
		printf("    movzx rax, al\n"); // alをゼロ拡張

		// 最後の比較
		printf(".Lcond%d:\n", lend);
		mov(RDI, "2");
		cmp(new_primitive_type(INT), new_primitive_type(INT));
		printf("    sete al\n"); // 2と等しいかをalに格納
		printf("    movzx rax, al\n"); // alをゼロ拡張
	}
	else if (node->kind == ND_COND_OR)
	{
		lend = jumpLabelCount++;

		// 左辺を評価
		equality(node->lhs);
		mov(RDI, "0");
		cmp(node->lhs->type, new_primitive_type(INT));
		printf("    setne al\n"); // 0と等しくないかをalに格納
		printf("    movzx rax, al\n"); // alをゼロ拡張
		printf("    jne .Lcond%d\n", lend); // 0以外ならスキップ

		// 右辺を評価
		conditional(node->rhs);
		mov(RDI, "0");
		cmp(node->rhs->type, new_primitive_type(INT));
		printf("    setne al\n"); // 0と等しくないかをalに格納
		printf("    movzx rax, al\n"); // alをゼロ拡張

		printf(".Lcond%d:\n", lend);
	}
}

static void	load_lval(Node *node)
{
	if (node->kind == ND_LVAR)
		lval(node);
	else if (node->kind == ND_LVAR_GLOBAL)
		load_global(node);
	else if (node->kind == ND_DEREF)
		expr(node->lhs);// ここもDEREFと同じようにやってる！！！！！
	else if (node->kind == ND_STRUCT_VALUE)
		arrow(node, true);
	else if (node->kind == ND_STRUCT_PTR_VALUE)
		arrow(node, true);
	else
		error("左辺値が識別子かアドレスではありません");
}

static void	assign(Node *node)
{
	switch (node->kind)
	{
		case ND_ASSIGN:
		case ND_COMP_ADD:
		case ND_COMP_SUB:
		case ND_COMP_MUL:
		case ND_COMP_DIV:
		case ND_COMP_MOD:
			break;
		default:
			conditional(node);
			return;
	}

	//printf("#ASSIGN %d\n", node->lhs->kind);
	load_lval(node->lhs);	
	push();

	// TODO もっといい感じにやりたい
	switch (node->kind)
	{
		case ND_ASSIGN:
			expr(node->rhs);
			break ;
		case ND_COMP_ADD:
			push();

			//fprintf(stderr, "from %d\n", node->rhs->kind);
			add_check_pointer(node->type, &node->lhs, &node->rhs, false);
			//fprintf(stderr, "kind %d\n", node->rhs->kind);

			expr(node->rhs);
			pop(RDI);
			if (node->type->ty == ARRAY)
				printf("    add rax, rdi\n");
			else
				printf("    add rax, [rdi]\n");
			break ;
		case ND_COMP_SUB:
			push();
			add_check_pointer(node->type, &node->lhs, &node->rhs, false);

			expr(node->rhs);
			mov(RDI, RAX);
			pop(RAX);
			if (node->type->ty == ARRAY)
				printf("    sub rax, rdi\n");
			else
			{
				printf("    mov rax, [rax]\n");
				printf("    sub rax, rdi\n");
			}
			break ;
		case ND_COMP_MUL:
			push();
			expr(node->rhs);
			mov(RDI, RAX);
			pop(RAX);
			// TODO 整数だけ？
			printf("    mov rax, [rax]\n");
			printf("    imul rax, rdi\n");
			break ;
		case ND_COMP_DIV:
			push();
			expr(node->rhs);
			mov(RDI, RAX);
			pop(RAX);
			// TODO 整数だけ？
			printf("    mov rax, [rax]\n");
			printf("    cdq\n");
			printf("    idiv edi\n");
			break ;
		case ND_COMP_MOD:
			push();
			expr(node->rhs);
			mov(RDI, RAX);
			pop(RAX);
			// TODO 整数だけ？
			printf("    mov rax, [rax]\n");
			printf("    cdq\n");
			printf("    idiv edi\n");
			mov(RAX, RDX);
			break ;
		default:
			break;
	}
	pop("r10");

	// storeする
	if (node->type->ty == ARRAY)
		// TODO これOKなの？
		// ARRAYに対する代入がうまくいかない気がする
		store_value(8);
	else if(node->type->ty == STRUCT)
		store_ptr(type_size(node->type), false);
	else
		store_value(type_size(node->type));
}

static void	expr(Node *node)
{
	assign(node);
}

extern int	switchCaseCount;

static void stmt(Node *node)
{
	int		lend;
	int		lbegin;
	int		lbegin2;
	SBData	*sbdata;

	switch (node->kind)
	{
		case ND_RETURN:
		case ND_IF:
		case ND_WHILE:
		case ND_DOWHILE:
		case ND_FOR:
		case ND_SWITCH:
		case ND_CASE:
		case ND_BLOCK:
		case ND_BREAK:
		case ND_CONTINUE:
		case ND_DEFAULT:
			break;
		default:
			expr(node);
			return;
	}

	switch (node->kind)
	{
		case ND_RETURN:
			if (node->lhs != NULL)
				expr(node->lhs);
			epilogue();
			stack_count += 8; // rbpをpopしたけれど、epilogueでもpopするので+8
			return;
		case ND_IF:
			// if
			expr(node->lhs);
			mov(RDI, "0");
			cmp(node->lhs->type, new_primitive_type(INT));

			lend = jumpLabelCount++;

			if (node->elsif != NULL)
			{	
				int lelse = jumpLabelCount++;
				printf("    je .Lelse%d\n", lelse);
				stmt(node->rhs);
				printf("    jmp .Lend%d\n", lend);

				// else if
				printf(".Lelse%d:\n", lelse);
				stmt(node->elsif);
			}
			else if (node->els == NULL)
			{
				printf("    je .Lend%d\n", lend);
				stmt(node->rhs);
			}
			else
			{
				int lelse = jumpLabelCount++;
				printf("    je .Lelse%d\n", lelse);
				stmt(node->rhs);
				printf("    jmp .Lend%d\n", lend);

				// else
				printf(".Lelse%d:\n", lelse);
				stmt(node->els);
			}
			printf(".Lend%d:\n", lend);
			return;
		case ND_WHILE:
			lbegin = jumpLabelCount++;
			lend = jumpLabelCount++;
			
			printf(".Lbegin%d:\n", lbegin); // continue先
			
			// if
			expr(node->lhs);
			mov(RDI, "0");
			cmp(node->lhs->type, new_primitive_type(INT));
			printf("    je .Lend%d\n", lend);
			
			// while block
			sb_forwhile_start(lbegin, lend);
			if (node->rhs != NULL)
				stmt(node->rhs);
			sb_end();
			
			// next
			printf("    jmp .Lbegin%d\n", lbegin);
			
			// end
			printf(".Lend%d:\n", lend); //break先
			return;
		case ND_DOWHILE:
			lbegin = jumpLabelCount++;
			lbegin2 = jumpLabelCount++;
			lend = jumpLabelCount++;
			
			printf(".Lbegin%d:\n", lbegin);

			// while block
			sb_forwhile_start(lbegin2, lend);
			if (node->lhs != NULL)
				stmt(node->lhs);
			sb_end();

			// if
			printf(".Lbegin%d:\n", lbegin2); // continueで飛ぶ先
			expr(node->rhs);
			mov(RDI, "0");
			cmp(node->rhs->type, new_primitive_type(INT));
			printf("    jne .Lbegin%d\n", lbegin);
			printf(".Lend%d:\n", lend); // break先
			return;
		case ND_FOR:
			lbegin = jumpLabelCount++;
			lbegin2 = jumpLabelCount++;
			lend = jumpLabelCount++;
			
			// init
			if (node->for_init != NULL)
				expr(node->for_init);

			printf(".Lbegin%d:\n", lbegin);
			
			// if
			if(node->for_if != NULL)
			{
				expr(node->for_if);
				mov(RDI, "0");
				cmp(node->for_if->type, new_primitive_type(INT));
				printf("    je .Lend%d\n", lend);
			}

			// for-block
			sb_forwhile_start(lbegin2, lend);
			if (node->lhs != NULL)
				stmt(node->lhs);
			sb_end();

			printf(".Lbegin%d:\n", lbegin2); // continue先
			// next
			if(node->for_next != NULL)
				expr(node->for_next);

			printf("    jmp .Lbegin%d\n", lbegin);
			
			//end
			printf(".Lend%d:\n", lend); // break先
			return;
		case ND_SWITCH:
			lbegin = switchCaseCount++;
			lend = jumpLabelCount++;

			// 評価
			expr(node->lhs);
			printf("    mov [rsp - 8], rax\n"); //結果を格納

			// if
			printf("    # switch def:%d, end:%d\n", lbegin, lend);
			for (SwitchCase *tmp = node->switch_cases; tmp; tmp = tmp->next)
			{
				printf("    mov rax, [rsp - 8]\n");
				movi(RDI, tmp->value);
				cmp(node->lhs->type, node->lhs->type);
				printf("    je .Lswitch%d\n", tmp->label);
			}
			// defaultかendに飛ばす
			if (node->switch_has_default)
				printf("    jmp .Lswitch%d\n", lbegin);
			else
				printf("    jmp .Lend%d\n", lend);

			printf("    # switch in\n");
			// 文を出力
			sb_switch_start(node->lhs->type, lend, lbegin);
			stmt(node->rhs);
			sb_end();

			printf(".Lend%d:\n", lend);
			return ;
		case ND_CASE:
			printf(".Lswitch%d: # case %d\n", node->switch_label, node->val);
			return ;
		case ND_BREAK:
			sbdata = sb_peek();
			// 一応チェック
			if (sbdata == NULL)
				error("breakに対応する文が見つかりません");
			printf("jmp .Lend%d # break\n", sbdata->endlabel);
			return ;
		case ND_CONTINUE:
			sbdata = sb_search(false);
			// 一応チェック
			if (sbdata == NULL)
				error("continueに対応する文が見つかりません");
			printf("jmp .Lbegin%d # continue\n", sbdata->startlabel);
			return ;
		case ND_DEFAULT:
			sbdata = sb_search(true);
			// 一応チェック
			if (sbdata == NULL)
				error("defaultに対応する文が見つかりません");
			printf(".Lswitch%d: # default\n", sbdata->defaultLabel);
			return ;
		case ND_BLOCK:
			while(node != NULL)
			{
				if (node->lhs == NULL)
					break;
				stmt(node->lhs);
				node = node->rhs;
			}
			return;
		default:
			break;
	}
}

static void	funcdef(Node *node)
{
	char	*funcname;

	funcname = strndup(node->fname, node->flen);
	stack_count = 0;

	printf(".globl _%s\n", funcname);
	printf("_%s:\n", funcname);	
	prologue();


	if (node->is_variable_argument)
	{
		printf("sub rsp, 400\n");
		printf("movaps	xmmword ptr [rbp - 400], xmm7\n");
		printf("movaps	xmmword ptr [rbp - 384], xmm6\n");
		printf("movaps	xmmword ptr [rbp - 368], xmm5\n");
		printf("movaps	xmmword ptr [rbp - 352], xmm4\n");
		printf("movaps	xmmword ptr [rbp - 336], xmm3\n");
		printf("movaps	xmmword ptr [rbp - 320], xmm2\n");
		printf("movaps	xmmword ptr [rbp - 304], xmm1\n");
		printf("movaps	xmmword ptr [rbp - 288], xmm0\n");
		printf("mov	qword ptr [rbp - 272], r9\n ");
		printf("mov	qword ptr [rbp - 264], r8\n");
		printf("mov	qword ptr [rbp - 256], rcx\n");
		printf("mov	qword ptr [rbp - 248], rdx\n");
		printf("mov	qword ptr [rbp - 240], rsi\n");
		printf("mov	qword ptr [rbp - 232], rdi\n");
    	
		printf("mov	rcx, qword ptr [rbp - 232]\n");
		printf("mov	rax, qword ptr [rbp - 240]\n");
		printf("mov	rdx, qword ptr [rbp - 248]\n");
		printf("mov	rsi, qword ptr [rbp - 256]\n");
		printf("mov	rdi, qword ptr [rbp - 264]\n");
		printf("mov	r8, qword ptr [rbp - 272] \n");
		printf("mov	qword ptr [rbp - 184], r8\n");
		printf("mov	qword ptr [rbp - 192], rdi\n");
		printf("mov	qword ptr [rbp - 200], rsi\n");
		printf("mov	qword ptr [rbp - 208], rdx\n");
		printf("mov	qword ptr [rbp - 216], rax\n");

    }

	if (node->locals != NULL)
	{
		int maxoff = 0;
		for (LVar *tmp = node->locals; tmp; tmp = tmp->next)
		{
			printf("# VAR %s %d\n", strndup(tmp->name, tmp->len), tmp->offset);
			maxoff = max(maxoff, tmp->offset);
		}
		node->stack_size = align_to(maxoff, 8);
	}
	else
		node->stack_size = 0;

	stack_count += node->stack_size; // stack_sizeを初期化

	printf("# STACKSIZE : %d\n", node->stack_size);

	if (node->stack_size != 0)
	{
		printf("    sub rsp, %d\n", node->stack_size);// stack_size

		for (LVar *tmp = node->locals; tmp; tmp = tmp->next)
		{
			if (!tmp->is_arg)
				continue ;
			printf("# ARG %s\n", strndup(tmp->name, tmp->len));
			if (tmp->arg_regindex != -1)
			{
				int index = tmp->arg_regindex;
				int size = align_to(type_size(tmp->type), 8);

				mov(R10, RBP);
				printf("    sub %s, %d\n", R10, tmp->offset);

				// とりあえず、かならず8byte境界になっている
				while (size >= 8)
				{
					mov(RAX, arg_regs[index--]);
					store_value(8);
					size -= 8;
					if (size != 0)
						printf("    add %s, 8\n", R10);
				}
			}
		}
		printf("# ARG_END\n");
	}

	stmt(node->lhs);
	epilogue();

	stack_count -= node->stack_size;
	printf("#count %d\n", stack_count);
	
	if (stack_count != 0)
		error("stack_countが0ではありません");
}

static void globaldef(Node *node)
{
	char	*name;

	if (node->is_extern)
		return ;
	name = strndup(node->var_name, node->var_name_len);
	printf(".globl %s\n", name);
	printf("    .zerofill __DATA,__common,_%s,%d,2\n",
		name,
		type_size(node->type));
}

static void	filescope(Node *node)
{
	if (node->kind == ND_FUNCDEF)
		funcdef(node);
	else if (node->kind == ND_DEFVAR_GLOBAL)
		globaldef(node);
	else
		stmt(node);
}

void	gen(Node *node)
{
	if (node->kind == ND_PROTOTYPE
	|| node->kind == ND_DEFVAR
	|| node->kind == ND_STRUCT_DEF
	|| node->kind == ND_TYPEDEF
	|| node->kind == ND_ENUM_DEF)
		return;
	filescope(node);
}
