##
## EPITECH PROJECT, 2023
## B-FUN-400-PAR-4-1-compressor-vincent.shao
## File description:
## Makefile
##

NAME	=	imageCompressor

path	=	$(shell stack path --local-install-root)

all:	$(NAME)

$(NAME):
	stack build
	cp $(path)/bin/$(NAME) .

tests_run:
	stack test --coverage

clean:
	stack clean
	$(RM) src/*hi

fclean:	clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all clean fclean re tests_run
