voidF::(x: int, y: int, z:int) {
	x = x * y * z;
	return;
}

mult2::(x: int) int {
	return x * 2;
}

main::(argc: int, argv: string) int {
	x : int = 3;
	y : int = mult2(x);
	z : int = mult2(y);
	voidF(x, y, z);
	return 0;
}