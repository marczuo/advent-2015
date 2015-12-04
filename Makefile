ifeq ($(shell uname), Darwin)
	# Mac OS X
	TIMEOUT = "gtimeout"
else
	TIMEOUT = "timeout"
endif

day% : day%.hs
	ghc -o $@.out $<

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
	number=1 ; while [[ $$number -le 25 ]] ; do \
	   if [ -f "day$${number}.hs" ] ; then \
		   echo "Testing day$$number" ; \
		   $(TIMEOUT) 1 make test$$number ; \
		   if [ $$? -eq 124 ] ; then \
		   		echo "Timeout during test$${number}: please run test individually with make test$${number}" ; \
		   fi ; \
	   fi ; \
	   if [ -f "day$${number}.hs" ] ; then \
		   echo "Testing target day$${number}p2" ; \
		   $(TIMEOUT) 1 make test$${number}p2 ; \
		   if [ $$? -eq 124 ] ; then \
		   		echo "Timeout during test$${number}p2: please run test individually with make test$${number}p2" ; \
		   fi ; \
	   fi ; \
	   (( number = number + 1 )) ; \
   done

.PHONY: clean
clean: 
	rm -f *.out *.o *.hi
