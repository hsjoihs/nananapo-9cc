#include <stdio.h>

int	main(void)
{
	char	*s;
	int		i;

	s = "HelloWorld";
	i = 0;
	while (i < 10)
		printf("%c\n", s[i++]);
}
