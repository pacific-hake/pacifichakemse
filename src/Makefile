all: runHakeassessment.so

runHakeassessment.so: runHakeassessment.cpp
	$(R_HOME)/bin/Rscript --vanilla -e "TMB::compile('runHakeassessment.cpp', '-O1 -g', safebounds = FALSE, safeunload = FALSE, DLLFLAGS = '')"

clean:
	rm -rf *.o
	rm -rf *.so
