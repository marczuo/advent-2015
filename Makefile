# Advent of Code 21

# Use timeout to prevent tests running for too long
# Requires coreutils
ifeq ($(shell uname), Darwin)
	# Mac OS X
	TIMEOUT = "gtimeout"
else
	TIMEOUT = "timeout"
endif

day% : day%.hs
	ghc -o $@.out $<

# {{{ testall subroutines

# The following two goals are used by testall to log the results.
# They should not be called directly.  

testallsubroutine%p2 : day%p2
	@./$<.out input$* | awk '{print "$<\t" $$0}' | tee -a testall.log

testallsubroutine% : day%
	@./$<.out input$* | awk '{print "$<\t" $$0}' | tee -a testall.log

# }}}


test%p2 : day%p2
	./$<.out input$*

test% : day%
	./$<.out input$*

.PHONY: all
all :
	number=1 ; while [[ $$number -le 25 ]] ; do \
	   if [ -f "day$${number}.hs" ] ; then \
		   echo "Making target day$$number" ; \
		   make day$$number ; \
	   fi ; \
	   if [ -f "day$${number}.hs" ] ; then \
		   echo "Making target day$${number}p2" ; \
		   make day$${number}p2 ; \
	   fi ; \
	   (( number = number + 1 )) ; \
   done

.PHONY: testall
testall :
	> testall.log
	number=1 ; while [[ $$number -le 25 ]] ; do \
	   if [ -f "day$${number}.hs" ] ; then \
		   echo "Testing day$$number" ; \
		   $(TIMEOUT) 1 make testallsubroutine$$number 2> /dev/null ; \
		   if [ $$? -eq 124 ] ; then \
		   		echo "Timeout during test$${number}: please run test individually with make test$${number}" ; \
				echo "test$${number}\ttimeout" >> testall.log ; \
		   fi ; \
	   fi ; \
	   if [ -f "day$${number}.hs" ] ; then \
		   echo "Testing day$${number}p2" ; \
		   $(TIMEOUT) 1 make testallsubroutine$${number}p2 2> /dev/null ; \
		   if [ $$? -eq 124 ] ; then \
		   		echo "Timeout during test$${number}p2: please run test individually with make test$${number}p2" ; \
				echo "test$${number}p2\ttimeout" >> testall.log ; \
		   fi ; \
	   fi ; \
	   (( number = number + 1 )) ; \
	done
	@echo "Done; you may see the results with cat testall.log" 

.PHONY: clean
clean: 
	rm -f *.out *.o *.hi
	rm -f */*.out */*.o */*.hi
	rm -f testall.log
