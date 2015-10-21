
function fib(z, u) {
	var v;

	if (z < 3) {
		v = u + 1;
	} else {
		v = fib(z-1, u);
		v = fib(z-2, v);
	}
	
	return v;
}

y = fib(10, 0);