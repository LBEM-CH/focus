ctffind3 (version 3.4 and higher) and ctftilt (version 1.5 and higher) can now make use of openMP. Use the _mp makefiles to compile, for example

make -f Makefile_linux_mp

This will produce executables

ctffind3_mp.exe
ctftilt_mp.exe

To use more than one processor, you will need to set the environmental variable NCPUS. For example, when using the csh shell:

setenv NCPUS 8

to use 8 processors. With 8 processors, ctffind3 runs about 6 times faster and ctftilt about 4 times faster.
