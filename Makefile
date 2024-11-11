all:
	@echo "Generating C code from Fut code..."
	@fut -o fut-out/ric.c src/*.fu
	@echo "Compiling C code..."
	@gcc -o ric fut-out/* -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -lglib-2.0