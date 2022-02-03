#   This file is part of rml. \
\
    rml is free software: you can redistribute it and/or modify \
    it under the terms of the GNU Lesser General Public License as published by \
    the Free Software Foundation, either version 3 of the License, or \
    any later version. \
\
    rml is distributed in the hope that it will be useful, \
    but WITHOUT ANY WARRANTY; without even the implied warranty of \
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \
    GNU Lesser General Public License for more details. \
\
    You should have received a copy of the GNU Lesser General Public License \
    along with rml. If not, see <https://www.gnu.org/licenses/>.

EMU=../../emu
CC=gcc

all: test1.out test2.out test3.out test4.out test5.out
	./test1.out
	./test2.out
	./test3.out
	./test4.out
	./test5.out
test1.out: test1.c test1.emu
	$(EMU) test1.emu -o test1.o
	$(CC) test1.c test1.o -o test1.out
test2.out: test2.c test2.emu
	$(EMU) test2.emu -o test2.o
	$(CC) test2.c test2.o -o test2.out
test3.out: test3.c test3a.emu test3b.emu
	$(EMU) test3a.emu test3b.emu -o test3.o
	$(CC) test3.c test3.o -o test3.out
test4.out: test4.c test4.emu
	$(EMU) test4.emu -o test4.o
	$(CC) test4.c test4.o -o test4.out
test5.out: test5.c test5.emu
	$(EMU) test5.emu -o test5.o
	$(CC) test5.c test5.o -o test5.out
test6.out: test6.c test6.emu
	$(EMU) test6.emu -o test6.o
	$(CC) test6.c test6.o -o test6.out
clean:
	rm *.o *.out

.PHONY: clean
