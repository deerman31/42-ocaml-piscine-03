NAME = a.out

SOURCES = Deck.ml main.ml

CMI = Deck.cmi
CMX = $(SOURCES:.ml=.cmx)
OBJS = $(CMX)

BUILD_DIR = build

MLI = Deck.mli

NC = ocamlopt
NC_FLAGS = -I $(BUILD_DIR)

RM = rm -f
MKDIR = mkdir -p

all: $(NAME)

$(NAME): $(BUILD_DIR) $(CMI) $(OBJS)
	$(NC) $(NC_FLAGS) -o $(NAME) $(addprefix $(BUILD_DIR)/,$(OBJS))

$(BUILD_DIR):
	$(MKDIR) $(BUILD_DIR)

%.cmi: %.mli
	$(NC) $(NC_FLAGS) -c $< -o $(BUILD_DIR)/$@

%.cmx: %.ml
	$(NC) $(NC_FLAGS) -c $< -o $(BUILD_DIR)/$@

clean:
	$(RM) -r $(BUILD_DIR)

fclean: clean
	$(RM) $(NAME)

re: fclean all

.PHONY: all clean fclean re