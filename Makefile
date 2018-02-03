QUICKLISP_HOME=$(HOME)/quicklisp/

all: planetfeed

deps.txt: planetfeed.asd
	sbcl --non-interactive --no-userinit --no-sysinit \
	  	--load $(QUICKLISP_HOME)/setup.lisp --load planetfeed.asd \
		--eval '(ql:quickload :planetfeed)' \
		--eval '(ql:write-asdf-manifest-file "deps.txt")'

planetfeed: *.lisp *.asd deps.txt
	buildapp --output planetfeed --entry planetfeed::cli \
		--manifest-file deps.txt \
		--asdf-path . \
		--load-system planetfeed 

clean:
	rm -f planetfeed deps.txt
