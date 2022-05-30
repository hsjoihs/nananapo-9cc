#include "prlib.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

Token	*add_token(TokenizeEnv *env, TokenKind kind, char *str, int len)
{
	Token	*tok;
	Token	*tmp:

	tok = (Token *)malloc(sizeof(Token));
	tok->str = str;
	tok->len = len;
	tok->is_directive = false;
	tok->is_dq = false;
	tok->next = NULL;
	if (env->token == NULL)
		env->token = tok;
	else
	{
		tmp = env->token;
		while (tmp->next != NULL)
			tmp = tmp->next;
		tmp->next = tok;
	}
	return (tok);
}

bool	tokenize_reserved_word(TokenizeEnv *env)
{
	int	len;

	len = is_reserved_word(env->str);
	if (len == 0)
		return (false);
	if (tokenize_comment(env, len))
		return (true);
	add_token(env, TK_RESERVED, env->str, len);
	env->str += len;
	return (true);
}

bool	tokenize_new_line(TokenizeEnv *env)
{
	if (
}

Token	*tokenize(char *str)
{
	TokenizeEnv	env;
	Node		*node;

	env.str = str;
	env.token = NULL;
	env.can_define_dir = true;
	env.str = skip_space(env.str);
	while (env.str != NULL || *env.str != '\0')
	{
		env.str = skip_space(env.str);
		if (tokenize_new_line(&env);
			|| tokenize_reserved_word(&env)
			|| tokenize_ident(&env)
			|| tokenize_number(&env)
			|| tokenize_directive(&env)
			|| tokenize_str_literal(&env)
			|| tokenize_str_literal(&env))
			continue ;
		error_at(env.str, "字句解析に失敗しました(tokenize)");
	}
	add_token(&env, TK_EOF, env.str, 0);
	return (env.token);
}
