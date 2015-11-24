
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

function s(x,y) {
	var t;
	t = fib(x+y, 0)
	if (v == 1) {
		t = s(x-y, y-x)
	}
	return t
}

y = fib(10, 0);