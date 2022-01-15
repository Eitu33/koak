##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

all:
		stack build --copy-bins --local-bin-path . --allow-different-user

clean:
		rm -rf .stack-work

fclean:		clean
		rm -rf koak

re:		fclean all

.PHONY:		all clean fclean re
