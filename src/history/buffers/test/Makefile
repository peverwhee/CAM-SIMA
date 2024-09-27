SHELL           = /bin/sh

INCFLAG = -I
INCPATH += $(INCFLAG).
FCFLAGS += -g

SRCPATH = ../src
HASHPATH = $(SRCPATH)/hash
UTILPATH = $(SRCPATH)/util

HASHOBJS = hist_hashable.o hist_hash_table.o
HISTOBJS = hist_buffer.o hist_field.o hist_api.o

# Make sure we have a log file
ifeq ($(LOGFILE),)
LOGFILE := hist_test.log
endif

# TARGETS

hist_msg_handler.o: $(UTILPATH)/hist_msg_handler.F90
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

hist_hashable.o: $(HASHPATH)/hist_hashable.F90
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

hist_hash_table.o: $(HASHPATH)/hist_hash_table.F90
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

hist_buffer.o: $(SRCPATH)/hist_buffer.F90 hist_hashable.o hist_msg_handler.o
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

hist_field.o: $(SRCPATH)/hist_field.F90 hist_buffer.o hist_hashable.o
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

hist_api.o: $(SRCPATH)/hist_api.F90 hist_field.o hist_buffer.o
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

test_hash_table: test_hash.F90 $(HASHOBJS)
	@echo "${FC} ${FCFLAGS} ${INCPATH} -o $@ $^" 2>&1 >> $(LOGFILE)
	@${FC} ${FCFLAGS} ${INCPATH} -o $@ $^ 2>&1 >> $(LOGFILE)

test_hist_buffer: test_hist_buffer.F90 $(HISTOBJS) hist_hashable.o  hist_msg_handler.o
	@echo "${FC} ${FCFLAGS} ${INCPATH} -o $@ $^" 2>&1 >> $(LOGFILE)
	@${FC} ${FCFLAGS} ${INCPATH} -o $@ $^ 2>&1 >> $(LOGFILE)

# CLEAN
clean:
	@rm -f *.o *.mod hist_test.log
	@rm -f test_hash_table test_hist_buffer
