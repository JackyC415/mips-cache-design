
########################################################################
# DO NOT MODIFY BELOW THIS LINE
########################################################################
EXEC := tips
SRCFILES := cachelogic.c tips.c cpu.c memory.c util.c nogui.c gui.c
OBJS := $(SRCFILES:.c=.o)
CC := gcc
CFLAGS := -g -Wall -std=c99 `pkg-config --cflags gtk+-2.0`
LDFLAGS := -g -Wall -std=c99 `pkg-config --libs gtk+-2.0`
ifneq (,$(findstring CYGWIN,$(shell uname)))
	CFLAGS += -DCYGWIN
	LDFLAGS += -DCYGWIN
	EXEC := $(EXEC).exe
endif
ifeq (,$(findstring LINUX,$(shell uname)))
	CFLAGS += -DLINUX
	LDFLAGS += -DLINUX
endif

$(EXEC): $(OBJS)
	$(CC) -Wall -g -o $(EXEC) $(OBJS) `pkg-config --cflags gtk+-2.0` `pkg-config --libs gtk+-2.0`

clean :
	\rm -rf *~ *.o $(EXEC)
