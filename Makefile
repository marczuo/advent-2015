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
	ghc -O2 -threaded -with-rtsopts=-N3 -o $@.out $<

test%p1 : day%
	./$<.out --part-one-only input$*

test%p2 : day%
	./$<.out --part-two-only input$*

test% : day%
	./$<.out input$*

.PHONY: all
all :
	number=1 ; while [[ $$number -le 25 ]] ; do \
	   if [ -f "day$${number}.hs" ] ; then \
		   echo "Making target day$$number" ; \
		   make day$$number ; \
	   fi ; \
	   (( number = number + 1 )) ; \
	done

.PHONY: clean
clean: 
	rm -f *.out *.o *.hi
	rm -f Local/*/*.hi Local/*/*.o
	rm -f testall.log
