##
## EPITECH PROJECT, 2024
## mypandoc
## File description:
## Makefile
##

NAME		=	mypandoc

STACK_PATH	=	$(shell stack path --local-install-root)
RM			=	rm -rf

$(NAME):
	stack build
	find . -name "*.sqlite3" -delete
	cp $(STACK_PATH)/bin/$(NAME)-exe ./$(NAME)

all:	$(NAME)

clean:
	stack clean

fclean:	clean
	@$(RM) $(NAME)

re:		fclean all

.PHONY:	all clean fclean re
