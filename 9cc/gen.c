#include "9cc.h"
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

static void	expr(Node *node);

int	jumpLabelCount = 0;

#define ARG_REG_COUNT 6
char *arg_regs[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

int stack_count = 0;

int	align_to(int n, int align)
{
	return (n + align - 1) / align * align;
}

void	push()
{
	stack_count += 8;
	printf("    %s %s # stack=%d\n", ASM_PUSH, RAX, stack_count);
}

void	pushi(int data)
{
	stack_count += 8;
	printf("    %s %d # stack=%d\n", ASM_PUSH, data, stack_count);
}

void	pop(char *reg)
{
	stack_count -= 8;
	printf("    pop %s # stack=%d\n", reg, stack_count);
}

void	mov(char *dst, char *from)
{
	printf("    %s %s, %s\n", ASM_MOV, dst, from);
}

void	movi(char *dst, int i)
{
	printf("    %s %s, %d\n", ASM_MOV, dst, i);
}

void	load_global(Node *node)
{
	char	*prefix;

	printf("    mov rax, [rip + _%s@GOTPCREL]\n",
		strndup(node->var_name, node->var_name_len));
}

void	comment(char *c)
{
	printf("# %s\n", c);
}

int	max(int a, int b)
{
	if (a > b)
		return a;
	return b;
}

void	init_stack_size(Node *node)
{
	int	i;

	i = -1;
	node->stack_size = 0;
	printf("    # lvar stack : ");
	for  (LVar *var = node->locals;var;var = var->next)
	{
		/* 必要な分だけsubする
		if (++i < node->argdef_count)
		{*/
			node->stack_size += max(8, type_size(var->type));
			printf("+ %d", max(8, type_size(var->type)));
		/*}
		else
		{
			node->stack_size += type_size(var->type);
			printf(" + %d", type_size(var->type));
		}*/
	}
	printf(" = %d\n", node->stack_size);
	node->stack_size = align_to(node->stack_size, 16);
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

static void	load(Type *type)
{
	
	if (type->ty == PTR)
	{
		/*
		if (type->ptr_to->ty == CHAR)
		{
			printf("movzx eax, BYTE PTR [%s]\n", RAX);
			return ;
		}
		else if (type->ptr_to->ty == INT)
		{
			printf("movzx eax, WORD PTR [%s]\n", RAX);
			return ;
		}
		*/
		printf("#PTR\n");
		mov(RAX, "[rax]");
		return ;
	}
	else if (type->ty == CHAR)
	{
		printf("#CHAR\n");
		printf("movzx eax, BYTE PTR [%s]\n", RAX);
		return ;
	}
	else if (type->ty == INT)
	{
		printf("#INT\n");
		printf("movzx eax, WORD PTR [%s]\n", RAX);
		return ;
	}
	else if (type->ty == ARRAY)
	{
		//printf("#ARR\n");
		return ;
	}
}

// 変数のアドレスをraxに移動する
static void	lval(Node *node)
{
	if (node->kind != ND_LVAR && node->kind != ND_LVAR_GLOBAL)
		error("代入の左辺値が変数ではありません %d", node->kind);
	
	mov(RAX, RBP);
	printf("    sub %s, %d\n", RAX, node->offset);
}

static void	call(Node *node)
{
	Node	*tmp = node->args;
	int		i;
	
	i = 0;
	while (tmp != NULL && i < ARG_REG_COUNT)
	{
		expr(tmp);
		mov(arg_regs[node->argdef_count - i - 1], RAX);
		tmp = tmp->next;
		i++;
	}
	
	bool	aligned = stack_count % 16 != 0;
	if (!aligned)
	{
		comment("allign");
		printf("# stack = %d\n", stack_count);
		pushi(1);
	}

	printf("    #call stack count %d\n", stack_count);

	printf("    call _%s\n", strndup(node->fname, node->flen));

	if (!aligned)
		pop("rdi");

	return;
}

static void	primary(Node *node)
{
	char *ch;

	switch (node->kind)
	{
		case ND_LVAR:
			lval(node);
			load(node->type);
			return;
		case ND_LVAR_GLOBAL:
			load_global(node);
			load(node->type);
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
		default:
			error("不明なノード %d", node->kind);
			break;
	}
}

static void unary(Node *node)
{
	switch (node->kind)
	{
		case ND_ADDR:
		case ND_DEREF:
			break;
		default:
			primary(node);
			return;
	}
	
	switch(node->kind)
	{
		case ND_ADDR:
			lval(node->lhs);
			break;
		case ND_DEREF:
			expr(node->lhs);
			load(node->type);
			break;
		default:
			break;
	}
}

static void	mul(Node *node)
{
	switch (node->kind)
	{
		case ND_MUL:
		case ND_DIV:
			break;
		default:
			unary(node);
			return;
	}

	expr(node->rhs);
	push();
	expr(node->lhs);
	pop("rdi");

	switch (node->kind)
	{
		case ND_MUL:
			printf("    imul rax, rdi\n");
			break;
		case ND_DIV:
			printf("    cqo\n");
			printf("    idiv rdi\n");
			break;
		default:
			break;
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

	// ポインタ
	if (node->type->ty == PTR || node->type->ty == ARRAY)
	{
		// 左辺をポインタ型にする
		if (node->rhs->type->ty == PTR || node->rhs->type->ty == ARRAY)
		{
			Node *tmp = node->lhs;
			node->lhs = node->rhs;
			node->rhs = tmp;
		}

		// 右辺を掛け算に置き換える
		Node *size_node = new_node(ND_NUM, NULL, NULL);
		size_node->val = type_size(node->lhs->type->ptr_to);
		size_node->type = new_primitive_type(INT);
		node->rhs = new_node(ND_MUL, node->rhs, size_node);
		size_node->type = new_primitive_type(INT);
	}

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
			printf("    cmp rax, rdi\n");
			printf("    setl al\n");
			printf("    movzx rax, al\n");
			break;
		case ND_LESSEQ:
			printf("    cmp rax, rdi\n");
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
			printf("    cmp rdi, rax\n");
			printf("    sete al\n");
			printf("    movzx rax, al\n");
			break;
		case ND_NEQUAL:
			printf("    cmp rdi, rax\n");
			printf("    setne al\n");
			printf("    movzx rax, al\n");
			break;
		default:
			break;
	}
}

static void	assign(Node *node)
{
	if (node->kind != ND_ASSIGN)
	{
		equality(node);
		return;
	}
	
	if (node->lhs->kind == ND_LVAR)
		lval(node->lhs);
	else if (node->lhs->kind == ND_LVAR_GLOBAL)
		load_global(node->lhs);
	else if (node->lhs->kind == ND_DEREF)
		expr(node->lhs->lhs);
	else
		error("代入の左辺値が識別子かアドレスではありません");

	push();
	expr(node->rhs);

	// レジスタの左辺の型に合わせる
	char *reg = type_regname(node->lhs->type);
	pop("rdi");
	mov("[rdi]", reg);
}

static void	expr(Node *node)
{
	assign(node);
}

static void stmt(Node *node)
{
	int	lend;
	int	lbegin;

	switch (node->kind)
	{
		case ND_RETURN:
		case ND_IF:
		case ND_WHILE:
		case ND_FOR:
		case ND_BLOCK:
			break;
		default:
			expr(node);
			return;
	}

	switch (node->kind)
	{
		case ND_RETURN:
			expr(node->lhs);
			epilogue();
			stack_count += 8; // rbpをpopしたけれど、epilogueでもpopするので+8
			return;
		case ND_IF:
			// if
			expr(node->lhs);
			printf("    cmp rax, 0\n");

			lend = jumpLabelCount++;

			if (node->els == NULL)
			{
				printf("    je .Lend%d\n", lend);
				stmt(node->rhs);
			}
			else
			{
				int lelse = jumpLabelCount++;
				printf("    je .Lelse%d\n", lelse);
				
				// if stmt
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
			
			printf(".Lbegin%d:\n", lbegin);
			
			// if
			expr(node->lhs);
			printf("    cmp rax, 0\n");
			printf("    je .Lend%d\n", lend);
			
			// while block
			stmt(node->rhs);
			
			// next
			printf("    jmp .Lbegin%d\n", lbegin);
			
			// end
			printf(".Lend%d:\n", lend);
			return;
		case ND_FOR:
			lbegin = jumpLabelCount++;
			lend = jumpLabelCount++;
			
			// init
			if (node->for_init != NULL)
				expr(node->for_init);

			printf(".Lbegin%d:\n", lbegin);
			
			// if
			if(node->for_if != NULL)
			{
				expr(node->for_if);
				printf("    cmp rax, 0\n");
				printf("    je .Lend%d\n", lend);
			}

			// for-block
			stmt(node->lhs);

			// next
			if(node->for_next != NULL)
				expr(node->for_next);

			printf("    jmp .Lbegin%d\n", lbegin);
			
			//end
			printf(".Lend%d:\n", lend);
			return;
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
	int		i;

	printf("_%s:\n", strndup(node->fname, node->flen));	
	prologue();

	init_stack_size(node);
	stack_count += node->stack_size; // pushを初期化

	if (node->stack_size != 0)
	{
		printf("    sub rsp, %d\n", node->stack_size);// stack_size
		if (node->argdef_count != 0)
			mov(RAX, RBP);
		i = 0;
		LVar *tmp = node->locals;
		while (i < node->argdef_count && i < ARG_REG_COUNT)
		{
			printf("    sub rax, 8\n");
			mov("[rax]", arg_regs[i++]);
			tmp = tmp->next;
		}
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
/*	printf("_%s:\n    .zero %d\n",
		strndup(node->var_name, node->var_name_len),
		type_size(node->type));
*/
	printf(".zerofill __DATA,__common,_%s,%d,2\n",
		strndup(node->var_name, node->var_name_len),
		type_size(node->type));

}

static void	filescope(Node *node)
{
	if (node->kind == ND_FUNCDEF)
		funcdef(node);
	else if (node->kind == ND_GLOBAL)
		globaldef(node);
	else
		stmt(node);
}

void	gen(Node *node)
{
	if (node->kind == ND_PROTOTYPE || node->kind == ND_DEFVAR)
		return;
	filescope(node);
}
