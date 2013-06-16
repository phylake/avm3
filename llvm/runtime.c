#include "stdio.h"

int somec()
{
	printf("hello world\n");
	return 0;
}

/*

PDFS
marmot
programming c 3rd ed


public class Foo
{
	// 2 fixed properties and a baz();

	public var a:int; // multiname 79
	public var b:String; // multiname 103
	public function baz(a:int):void {} // multiname 40
}

public class Bar extends Foo
{
	public var c:int; // multiname 308
}
*/

struct FooClass
{
  
};

struct Foo
{
  struct FooClass * class;
};
