RM_F = perl -MExtUtils::Command -e rm_f

all: coord-trans.html surf-fit.html surf-fit2.html surf-fit3.html surf-fit4.html

NAME = coord-trans

$(NAME).html: $(NAME).pod
	podhtm -s Active.css $(NAME).pod

$(NAME).pod: $(NAME).podx
	podx $(NAME).podx

NAME = surf-fit

$(NAME).html: $(NAME).pod
	podhtm -s Active.css $(NAME).pod

$(NAME).pod: $(NAME).podx
	podx $(NAME).podx

NAME = surf-fit2

$(NAME).html: $(NAME).pod
	podhtm -s Active.css $(NAME).pod

$(NAME).pod: $(NAME).podx
	podx $(NAME).podx

NAME = surf-fit3

$(NAME).html: $(NAME).pod
	podhtm -s Active.css $(NAME).pod

$(NAME).pod: $(NAME).podx
	podx $(NAME).podx

NAME = surf-fit4

$(NAME).html: $(NAME).pod
	podhtm -s Active.css $(NAME).pod

$(NAME).pod: $(NAME).podx
	podx $(NAME).podx

clean::
	$(RM_F) *.pod *.tmp
