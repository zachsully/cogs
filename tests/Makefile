COGS=../dist/build/cogs/cogs

PROGS= literal.out \
       neg.out \
       add1.out \
       add2.out \
       addZeroes.out \
       exponent1.out \

test : $(PROGS)

%.out : %.cs
	$(COGS) symbolic $< -o $@

clean :
	rm *.out
