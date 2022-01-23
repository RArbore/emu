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

test1: test1.c test1.emu
	$(EMU) test1.emu
	$(CC) test1.c out.o -o test1.out
clean:
	rm *.o *.out
