voidF::(x: int, y: int, z:int) {
	x = x * y * z;
	return;
}

mult2::(x: int) int {
	return x * 2;
}

addMultAndDouble::(x: int, y: int) {
	acc: int = (x + y) * 2;
	return acc;
}

main::(argc: int) int {
	x : int = 3;
	y : int = x * (4 + 5);
	z : int = y * (y / x);
	return 0;
}