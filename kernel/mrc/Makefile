INSTALL = /usr/bin/install -c
SUBDIRS = source source/2dx_getlat source/ctf source/peaksearch source/endianness source/periodogram source/maximum_likelihood
COPY = \cp -r
MKDIR = \mkdir -p

all: $(SUBDIRS:=/all.target)
$(SUBDIRS:=/all.target): lib/all.target

install: 
	@$(MKDIR) $(root)$(prefix)
	@$(COPY) Makefile.in configure.in configure bin lib include source $(root)$(prefix)/

clean: $(SUBDIRS:=/clean.target)

%.target:
	@echo $(MAKE) $(MFLAGS) -C $(*D) $(*F)
	@[ -d $(*D) ] && $(MAKE) $(MFLAGS) -C $(*D) $(*F)

